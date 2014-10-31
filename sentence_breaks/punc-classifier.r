library(C50)
library(openNLP)
library(data.table)
require(NLP)
library(plyr)
library(intervals)

get.punctree <- function(posdir, treefile=NULL, train.prop=NA) {
	filenames <- list.files(posdir, pattern=".pos$", full.names=T)
	filelist <- as.list(filenames)
	names(filelist) <- basename(filenames)

	train.convs <- lapply(filelist, function(filename) {x <- load(filename); get(x)})	
	train.convs.dt <- rbindlist(train.convs[sample.int(length(train.convs))])	
	train.convs.dt$punc <- "CONT"
	train.convs.dt$punc[train.convs.dt$sent.end == T] <- "STOP"
	train.convs.dt$punc <- factor(train.convs.dt$punc)

	if (!is.na(train.prop)) {
		train.size <- floor(nrow(train.convs.dt) * train.prop)
		print(train.size)
		train.pos <- train.convs.dt[1:train.size]
		test.pos <- train.convs.dt[(train.size+1):nrow(train.convs.dt)]
	} else {
		train.pos <- train.convs.dt
	}


	punctree <- C5.0(punc ~ target+p1+p2+p3+n1+n2+n3, data=train.pos)

	if (!is.na(train.prop)) {
		punctreePred <- predict(punctree, test.pos)
		punctreeProbs <- data.table(test.pos, predict(punctree, test.pos, type ="prob"))

	} else {
		punctreeProbs <- NULL
	}

	if (!is.null(treefile)) {
		save(punctree, file=treefile)
		if (!is.null(punctreeProbs)) {
			save(punctreeProbs, file=paste(treefile,".test.Probs", sep=""))
		}
	}
	return(punctree)


}

apply.punctree <- function(treefile, test.pos=NULL, testfile=NULL) {
	x <- load(treefile)
	punctree <- get(x)

	if (is.null(test.pos) & !is.null(testfile)) {
		test.pos <- data.table(read.csv(testfile, header=F))
	}

	punctreePred <- predict(punctree, test.pos)
	punctreeProbs <- data.table(test.pos, predict(punctree, test.pos, type ="prob"))
	save(punctreeProbs, file="punctreeProbs")
}


demo.tokenizer <- function() {
	s <- paste(c("Pierre Vinken, 61 years old, will join the board as a ",
	"nonexecutive director Nov. 29.\n", "Mr. Vinken is chairman of Elsevier N.V.,
	", "the Dutch publishing group."), collapse = "")


	s <- as.String(s)
	sent_token_annotator <- Maxent_Sent_Token_Annotator()
	word_token_annotator <- Maxent_Word_Token_Annotator()
	a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))
	pos_tag_annotator <- Maxent_POS_Tag_Annotator()
	a3 <- annotate(s, pos_tag_annotator, a2)
	a3

}

get.stop.labelled.pos.conv <- function(filename) {
	x <- data.table(read.delim(filename, header=F))
	tpos <- get.pos.tags(x$V1, no.stops=F)
	tpos.words <- tpos[["pos.words"]]
	tpos.words <- data.table(tpos.words, nextword=c(tail(tpos.words$word,-1), NA))	
	tpos.words <- data.table(tpos.words, stop.next=tpos.words[,nextword=="."])	
	tpos.words <- tpos.words[!(pos %in% c(".", ",",":"))]

	tpos.cont <- get.pos.context(tpos.words)
	tpos.train <- data.table(tpos.cont, sent.end=tpos.words$stop.next, tpos.words$id, tpos.words$word)	

	return(tpos.train)

}


get.train.pos <- function(transdir) {
	filenames <- list.files(transdir, full.names=T)

	train.convs <- list() 
	for (filename in filenames) {
		print(filename)
		train.convs[[basename(filename)]] <- get.traintrans.conv(filename)
	}
	return(train.convs)

} 

word_space_tokenizer <-function(s) {
	s <- as.String(s)
	## Remove the last character (should be a period when using
	## sentences determined with the trivial sentence tokenizer).
	## Split on whitespace separators.
	m <- gregexpr("[^[:space:]]+", s)[[1L]]
	Span(m, m + attr(m, "match.length") - 1L)
}


get.pos.tags <- function(currwords, no.stops=F) {

	## convert word vector to string
	if (no.stops) {
		currwords <- gsub("\\.", "", currwords)		
	}
	s <- paste(tolower(currwords), collapse= " ")
	s <- as.String(s)

	 
	sent_token_annotator <- Maxent_Sent_Token_Annotator()
	word_token_annotator <- Maxent_Word_Token_Annotator()
	tok.words <- annotate(s, list(sent_token_annotator, word_token_annotator))
	pos_tag_annotator <- Maxent_POS_Tag_Annotator()
	pos.words <- annotate(s, pos_tag_annotator, tok.words)

	sw <- subset(pos.words, type == "sentence")
	sw.dt <- data.table(id=sw$id, start=sw$start, end=sw$end)
	sw.dt <- sw.dt[,list(start, end, sentence=substr(as.character(s), start,end)),by=id]
	
	pw <- subset(pos.words, type == "word")
	pw.dt <- data.table(id=pw$id, start=pw$start, end=pw$end, pos=unlist(pw$features))
	pw.dt <- pw.dt[,list(start, end, word=substr(as.character(s), start,end),pos=pos),by=id]
	print(pw.dt[grep("[A-Z]", pos, invert=T)])
	pw.dt[grep("[A-Z]", pos, invert=T)]$pos <- "UH"

	## Get original word positions in the string so we can map back to timing info.
	word_token_annotator <- Simple_Word_Token_Annotator(word_space_tokenizer)
	tok.words <- annotate(s, list(sent_token_annotator, word_token_annotator))
	tw <- subset(tok.words, type == "word")
	tw.dt <- data.table(id=tw$id, start=tw$start, end=tw$end)
	tw.dt <- tw.dt[,list(start, end, word=substr(as.character(s), start,end)),by=id]

	return(list(tok.words=tw.dt, pos.words=pw.dt, sentence=sw.dt))
}

get.pos.tok.overlap <- function(tw.dt, pw.dt, tw.id="wid") { 
	tw.int <- Intervals(as.matrix(tw.dt[,list(start,end)]))
	pw.int <- Intervals(pw.dt[,list(start,end)])
	v <- interval_overlap(tw.int, pw.int)
	names(v) <- tw.dt[[tw.id]]
	u <- ldply(v, function(x) {pw.dt[x]})
	print("end pos.tok.overlap")
	return(data.table(u))
}

get.pos.context <- function(pos.words, nprev=3, nnext=3) {
	curr.pos <- pos.words$pos
	pos.dt <- data.table(target=curr.pos)
	if (nprev > 0) {
		for (i in 1:nprev) {
			prev.pos <- c("None", head(curr.pos, -1))
			pos.dt <- data.table(prev.pos=prev.pos, pos.dt) 	
			setnames(pos.dt, c("prev.pos"), c(paste("p",i,sep="")))
			curr.pos <- prev.pos
		}
	}

	curr.pos <- pos.words$pos
	if (nprev > 0) {
		for (i in 1:nnext) {
			next.pos <- c(tail(curr.pos, -1), "None")
			pos.dt <- data.table(pos.dt, next.pos=next.pos) 	
			setnames(pos.dt, c("next.pos"), c(paste("n",i,sep="")))
			curr.pos <- next.pos
		}
	}

	return(pos.dt)
}

get.autopunc.words <- function(filename, punctree, word.var="wordId", start.var="wordStart", threshhold=0.5, word.id="word.id") {
	fstem <- basename(filename)
	print(filename)
	## Get words
	words.dt <- data.table(read.table(filename, header=T))
	print(paste("start.var:", start.var))
	print(names(words.dt))
	setnames(words.dt, start.var, c("wstart"))
	setnames(words.dt, word.id, c("niteid"))
	words.dt <- words.dt[order(wstart)]
	
	currwords <- words.dt[[word.var]]

	## get pos tags
	ptag.list <- get.pos.tags(currwords, no.stops=T) 
	#save(ptag.list, file=paste(fstem, ".ptag.list", sep=""))

	print("pos features")
	## Get POS features for decision tree   
	pos.words <- ptag.list[["pos.words"]] 
	pos.cont <- get.pos.context(pos.words)

	print("apply tree")
	## Apply decision tree
	punctreePred <- predict(punctree, pos.cont)
        #punctreeProbs <- data.table(pos.words, predict(punctree, pos.cont, type ="prob"), pred.class=predict(punctree,pos.cont,type ="class"))
        punctreeProbs <- data.table(pos.words, predict(punctree, pos.cont, type ="prob"))
	save(punctreeProbs, file=paste(fstem, ".tree.probs", sep=""))

	print("tok.words")
	## Match up with original word list, since pos.words have some contraction splitting.	
	tok.words <- ptag.list[["tok.words"]]

	## Add external ids if we have them
	tok.words$wid <- words.dt$niteid

	print("get.pos.tok.overlap")
	currw <- get.pos.tok.overlap(tok.words, punctreeProbs)
	setnames(currw, ".id", "wid")
	currw$id <- as.numeric(currw$id)

	print("Get new sentence labels")
	## Get new sentence labels
	sent.ends <- currw[STOP > threshhold, id]
	sent.starts <- c(min(currw[, id]), head(sent.ends+1, -1))
	s.dt <- data.table(sid=1:length(sent.ends), s.start=sent.starts, s.end=sent.ends)
	s.int <- Intervals(s.dt[,list(s.start, s.end)])
	w.int <- Intervals(currw[,list(id,id)]) 
	sw.list <- interval_overlap(s.int, w.int)
	names(sw.list) <- paste("sentence.", s.dt$sid, sep="")

	print("to data.table")
	currsw <- data.table(ldply(sw.list, function(x) {currw[x]}))
	setnames(currsw, ".id", "sid")

	setkey(currsw, wid)
	setkey(words.dt, niteid) 

	currsw <- words.dt[currsw]	
	currsw$sent.id <- paste(currsw$conv, currsw$spk, currsw$sid, sep=".")
	currsw.times <- currsw[,list(sent.start=min(wstart), sent.end=max(wend)),by=sent.id]
	setkey(currsw.times, sent.id)
	setkey(currsw, sent.id)

	currsw <- currsw.times[currsw]	

	print(paste(dirname(filename), "/", currsw$conv[1], ".autopunc.words.txt", sep=""))
	write.table(currsw, file=paste(dirname(filename), "/", currsw$conv[1], ".autopunc.words.txt", sep=""))
	print("HERE")
	return(currsw)
}

################################################
#args=(commandArgs(TRUE))
#print(args)
#if(length(args)==0){
#        stop("No arguments supplied. Exiting.")
#}
################################################
#transdir <- args[1]

#transdir <- "~/data/ted/traintrans/"




