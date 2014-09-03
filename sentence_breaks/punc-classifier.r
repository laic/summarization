library(C50)
library(openNLP)
library(data.table)
require(NLP)

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

get.traintrans.conv <- function(filename) {
	x <- data.table(read.delim(filename, header=F))
	tpos <- get.pos.tags(x$V1, no.stops=F)
	tpos.words <- tpos[["pos.words"]]
	tpos.words <- data.table(tpos.words, nextword=c(tail(tpos.words$word,-1), NA))	
	tpos.words <- data.table(tpos.words, stop.next=tpos.words[,nextword=="."])	
	tpos.words <- tpos.words[!(pos %in% c(".", ",",":"))]

	tpos.cont <- get.pos.context(tpos.words)
	tpos.train <- data.table(tpos.cont, sent.end=tpos.words$stop.next)	

	return(tpos.train)

}

get.train.pos <- function(transdir) {
	filenames <- list.files(transdir, full.names=T)

	train.convs <- list() 
	for (filename in filenames) {
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

	## Get original word positions in the string so we can map back to timing info.
	word_token_annotator <- Simple_Word_Token_Annotator(word_space_tokenizer)
	tok.words <- annotate(s, list(sent_token_annotator, word_token_annotator))
	tw <- subset(tok.words, type == "word")
	tw.dt <- data.table(id=tw$id, start=tw$start, end=tw$end)
	tw.dt <- tw.dt[,list(start, end, word=substr(as.character(s), start,end)),by=id]

	return(list(tok.words=tw.dt, pos.words=pw.dt, sentence=sw.dt))
}

get.pos.tok.overlap <- function(tw.dt, pw.dt) { 
	tw.int <- Intervals(tw.dt[,list(start,end)])
	pw.int <- Intervals(pw.dt[,list(start,end)])
	v <- interval_overlap(tw.int, pw.int)
	u <- unlist(lapply(v, function(x) {paste(c(pw.dt$word[x], pw.dt$pos[x]), collapse="")}))

	return(u)
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

################################################
#args=(commandArgs(TRUE))
#print(args)
#if(length(args)==0){
#        stop("No arguments supplied. Exiting.")
#}
################################################

#treefile<- args[1]
#wordfile<- args[1]





