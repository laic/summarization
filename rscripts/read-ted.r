## read stm files

library(stringi)
library(data.table)
source("../rscripts/f0basics.r")
source("../rscripts/nxt-proc.r")
#----------------------------------------------------
get.nwords.from.string <- function(x) {
	if (is.factor(x)) {
		x <- unlevel(x)
	}
	wlist <- strsplit(x, split=" ") 
	nwords <- unlist(lapply(wlist, function(words) {
		return(length(words[words != ""]))
	} )) 

	return(nwords)
} 

## Split an utterance string (trans) into a vector of words  
trans.to.word.vector <- function(trans) {
	if (is.factor(trans)) {
		print("unlevel")
		trans <- unlevel(trans)
	}
	wlist <- strsplit(trans, split=" ") 
	word.vector <- unlist(lapply(wlist, function(words) {
		words.clean <- words[words != ""]
		return(words.clean)
	} )) 
	return(word.vector)
} 

## Get a directory of .stm files
get.stm.trans.all <- function(transdir="~/data/ted/stm-ted2013/", infofile="~/data/ted/info/id_dname_year_sname", write.xml=F) {
	info.dt <- data.table(read.table(infofile, header=F, fill=T))
	setnames(info.dt, c("id","title","year","filestem"))

	transfiles <- data.table(filename=list.files(transdir, pattern=".stm"))
	stmtrans <- transfiles[, {
		currwords <- get.stm.trans(transdir, filename, info.dt)
		setnames(currwords, c("starttime", "endtime", "wstart", "wend"), 
			c("seg.start", "seg.end", "starttime", "endtime"))
		if (write.xml) {
			write.xml.words.nxt.all(currwords, wdir="~/data/ted/NXT-format/words/")
		}
		currwords
	}, by=filename]

	return(stmtrans)
}

## Fix irregularities in how ted transcripts/stm files mark speaker changes.
fix.trans.spks <- function(x) {	
	if (is.factor(x$trans)) { unlevel(x$trans) }

	x$trans <- gsub("[{]Roger two point oh[}]","{RE2}",x$trans)
	x$trans <- gsub("[{]President George W. Bush[}]","{GWB}",x$trans)
	x$trans <- gsub("[{]Sonny McDon W[}]","{SMW}",x$trans)
	x$trans <- gsub("[{]RG and AV[}]","{RGAV}",x$trans)
	x$trans <- gsub("[{]BJ / DJ[}]","{BJDJ}",x$trans)
	x$trans <- gsub("[{]iO.*[}]","{IO}",x$trans)

	x$trans <- gsub("[{]([A-Za-z])[A-Za-z'.]* ([A-Za-z])[a-z-]*[}]","{\\1\\2}",x$trans)
	x$trans <- gsub("[{]([A-Za-z])[a-z'.]*-[A-Za-z]* ([A-Za-z])[a-z-]*[}]","{\\1\\2}",x$trans)
	x$trans <- gsub("[{]([A-Za-z])[a-z'.]*[}]","{\\1}",x$trans)
	x$trans <- gsub("[{]([A-Za-z])[a-z'.]* ([a-zA-Z])[a-z]*([A-Z])[a-z]*[}]","{\\1\\2\\3}",x$trans)
	x$trans <- gsub("[{]([A-Za-z])[a-z'.]* ([A-Z])[a-z]*-([A-Z])[a-z]*[}]","{\\1\\2\\3}",x$trans)
	x$trans <- gsub("[{]([A-Za-z])[a-z'.]* ([A-Za-z])[a-z.']* ([A-Za-z])[a-z]*[}]","{\\1\\2\\3}",x$trans)
	x$trans <- gsub("[{]([A-Za-z])[a-z'.]* ([A-Za-z])[a-z.']* [a-z]* ([A-Za-z])[a-z]*[}]","{\\1\\2\\3}",x$trans)
	x$trans <- gsub("[{]([A-Za-z])[a-z'.]* ([A-Za-z])[A-Za-z']*[}]","{\\1\\2}",x$trans)
	x$trans <- gsub("[{]([A-Za-z])[a-z'.]* ([A-Za-z])[A-Za-z']*,.*[}]","{\\1\\2}",x$trans)

	return(x)
}


## Read an stm file.  This is quite ted specific.
get.stm.trans <- function(transdir="~/data/ted/stm-ted2013/", filename="0041.stm", info.dt, corpus="ted") {

	fname <- paste(transdir, "/", filename, sep="")
	currconv <- paste(toupper(corpus), gsub(".stm", "", filename), sep="") ## this is just another id?

	currid <- as.numeric(gsub(".stm", "", filename))
	print(fname)

	x <- data.table(conv=currconv, id=currid, read.delim(fname, header=F, quote="<"))
	setnames(x, c("conv", "id", "transid", "V2", "V3", "starttime", "endtime", "V6", "trans"))
	x$starttime <- as.double(x$starttime)
	x$endtime <- as.double(x$endtime)


	setkey(x, id)
	setkey(info.dt, id)
	x <- info.dt[x]	

	x <- fix.trans.spks(x) 

	## Determine the main speaker using info in the video long name.
	## This is a bit TED specific. We might want to change this.
	main.spk <- strsplit(unlevel(unique(x$filestem)), split="_")[[1]][1]
	main.spk <- gsub("[a-z]", "", main.spk)  

	## Clean segment level 
	x <- x[,list(conv=conv, longconv=filestem, id, spk=main.spk,
		 wav.file=wav.file, video.file=video.file, seg.id=1:length(trans), starttime, endtime, 
		nwords=get.nwords.from.string(trans), trans)]

	## get in individual words.
	x <- x[, list(word=trans.to.word.vector(trans)) 
			,by=list(conv, longconv, id,  spk, seg.id, starttime, endtime, nwords, wav.file, video.file)]

	## Add utterance position.  We don't actually know the start and end times 
	x <- x[,{
		list(word=word, utt.position=1:length(word),
			wstart=NA, wend=NA)
		},by=list(conv, longconv, id, spk, seg.id, starttime, endtime, nwords, wav.file, video.file)]

	## Detect which segments have a speaker change.  This is also TED specific?
	spk.change <- x[, grep("[{][A-Z]*[}]", word)]

	## Change the spk when it changes
	if (length(spk.change) > 0) {
		if (length(spk.change) > 1) {
			for (i in c(1:(length(spk.change)-1))) {
				currspk <- x$word[spk.change[i]]
				## The word before the next speaker change
				spk.end <- (spk.change[i+1]-1)

				## If the change happens within the turn, the end of 
				## the new speakers turn is the end of that turn.
				if (x$utt.position[spk.change[i]] >  1)  {
					currseg <- x$seg.id[spk.change[i]]
					curr.nwords <- x$nwords[spk.change[i]]
					spk.end <- x[, which(seg.id==currseg & utt.position==curr.nwords)]
				} 
				x$spk[c(spk.change[i]:spk.end)] <- currspk	
			}
		}
		## Deal with the end case
		i <- length(spk.change) 
		currspk <- x$word[spk.change[length(spk.change)]]
		spk.end <- length(x$spk)

		if (x$utt.position[spk.change[i]] >  1)  {
			currseg <- x$seg.id[spk.change[i]]
			curr.nwords <- x$nwords[spk.change[i]]
			spk.end <- x[, which(seg.id==currseg & utt.position==curr.nwords)]
		} 
		x$spk[spk.change[i]:spk.end] <- currspk	
	}

	## Get rid of speaker change markers from the word set
	x$spk <- gsub("[{}]", "", x$spk)
	x <- data.table(word.id=x[,paste(unique(conv),".", spk,".", "words",1:length(word),sep="")], x) 

	## add segment ids	
	x$seg.id <- x[,paste(unique(conv), spk, "seg", seg.id, sep=".")]

	return(x)
}




read.ctm.words <- function(ctmfile="~/data/ted/ted_aligned.ctm", infofile="~/data/ted/info/id_dname_year_sname") {

	ctm <- data.table(read.table(ctmfile, header=F, skip=1)) 
	xinfo <- data.table(read.table(infofile, header=F, fill=T))
	setnames(xinfo, c("id","title","year","conv"))
	setnames(ctm, c("title", "X", "starttime", "dur", "word", "X6"))
	setkey(xinfo, title)
	xwords <- xinfo[ctm]
	xwords$X <- NULL
	xwords$X6 <- NULL
	xwords <- data.table(xwords, endtime=xwords[,starttime+dur])

	return(xwords)

}

#-------------------------------------------------------------------------------------
# Write words out in NXT XML format

write.xml.words.nxt.all <- function(tedwords, wdir="~/data/ted/NXT-format/words/") {
	ddply(tedwords, .(conv), write.xml.words.nxt, wdir=wdir)	
}


write.xml.words.nxt <- function(xwords, wdir="~/data/ted/NXT-format/words/", suffix="words") {
	if (!("niteid" %in% xwords)) {
		xwords <- data.table(niteid=paste(xwords$conv, ".", xwords$spk, ".", suffix, 1:nrow(xwords), sep="") , xwords)
	}

	outfile <- paste(wdir, "/", unique(xwords$conv), ".A.", suffix, ".xml", sep="")				
	print(outfile)
	write("<?xml version=\"1.0\" encoding=\"ISO-8859-1\" standalone=\"yes\"?>", file=outfile, append=FALSE)
	write(paste("<nite:root nite:id=\"", unique(xwords$conv), ".A.", suffix, "\" xmlns:nite=\"http://nite.sourceforge.net/\">", sep="")			,
		 file=outfile, append=T) 

	xwords[, {
		curr <- paste("  <w nite:id=\"", niteid, "\" starttime=\"", starttime,  "\" endtime=\"", endtime, "\">", word, "</w>", sep="")
		write(curr, file=outfile, append=T)
		list(curr)
	},by=niteid] 

	write("</nite:root>", file=outfile, append=T)

}

write.segs.nxt.xml.all <- function(transdir="~/data/ted/stm-ted2013/", infofile="~/data/ted/info/id_dname_year_sname", wsuffix="words") {

	info.dt <- data.table(read.table(infofile, header=F, fill=T))
	setnames(info.dt, c("id","title","year","filestem"))
	info.dt <- data.table(info.dt, wav.file=paste(info.dt$filestem, "wav", sep="."), video.file=paste(info.dt$filestem,"mp4",sep="."))

	transfiles <- data.table(filename=list.files(transdir, pattern=".stm"))
	stmtrans <- transfiles[, {
		currwords <- get.stm.trans(transdir, filename, info.dt)
		write.segs.nxt.xml(currwords, wdir="~/data/ted/NXT-format/segs/", suffix="segs", wsuffix=wsuffix)
	}, by=filename]

	return(stmtrans)
}


write.segs.nxt.xml <- function(xsegs, wdir="~/data/ted/NXT-format/segs/", suffix="segs", wsuffix="words") {
	if (!("niteid" %in% xsegs)) {
		xsegs <- data.table(niteid=paste(xsegs$conv, ".A.", suffix, 1:nrow(xsegs), sep="") , xsegs)
	}

	currpart <- strsplit(unlevel(unique(xsegs$conv)), split="_")[[1]][1]
	outfile <- paste(wdir, "/", unique(xsegs$conv), ".A.", suffix, ".xml", sep="")				
	print(outfile)
	write("<?xml version=\"1.0\" encoding=\"ISO-8859-1\" standalone=\"yes\"?>", file=outfile, append=FALSE)
	write(paste("<nite:root nite:id=\"", unique(xsegs$conv), ".A.", suffix, "\" xmlns:nite=\"http://nite.sourceforge.net/\">", sep="")			,
		 file=outfile, append=T) 

	xsegs[, {
		currid <- paste("  <seg nite:id=\"", unique(seg.id), "\" starttime=\"", unique(starttime),  "\" endtime=\"", unique(endtime), "\" nxt_agent=\"A\" participant=\"",currpart, "\">", sep="")
		write(currid, file=outfile, append=T)
		wfile <- paste(unique(xsegs$conv), ".A.", wsuffix, ".xml", sep="")   	
		wordstr <- paste("\t<nite:child href=\"", wfile, "#", sep="")	

		if (length(word.id) == 0 ) { 
			wordstr <- ""
		} else if (length(word.id) == 1) {
			wordstr <- paste(wordstr, "id(",word.id[1],")\"/>", sep="")	
		} else {
			wordstr <- paste(wordstr, "id(",word.id[1],")..id(",word.id[length(word.id)], ")\"/>", sep="")	
		}

		write(wordstr, file=outfile, append=T)
		write("  </seg>", file=outfile, append=T)
		list(currid)
	},by=seg.id] 

	write("</nite:root>", file=outfile, append=T)

	

}


write.align.words.nxt.all <- function(tedwords, wdir="~/data/ted/words/") {
	ddply(tedwords, .(filestem), write.align.words.nxt, wdir=wdir)	
}


write.align.words.nxt <- function(xwords, wdir="~/data/ted/words/") {
	if (!("niteid" %in% xwords)) {
		xwords <- data.table(niteid=paste(xwords$filestem, ".A.alignwords", 1:nrow(xwords), sep="") , xwords)
	}

	outfile <- paste(wdir, "/", unique(xwords$filestem), ".A.alignwords", sep="")				
	print(outfile)
	write("<?xml version=\"1.0\" encoding=\"ISO-8859-1\" standalone=\"yes\"?>", file=outfile, append=FALSE)
	write(paste("<nite:root nite:id=\"", unique(xwords$filestem), ".A.alignwords\" xmlns:nite=\"http://nite.sourceforge.net/\">", sep=""),
		 file=outfile, append=T) 

	xwords[, {
		curr <- paste("  <w nite:id=\"", niteid, "\" starttime=\"", starttime,  "\" endtime=\"", endtime, "\">", word, "</w>", sep="")
		write(curr, file=outfile, append=T)
		list(curr)
	},by=niteid] 

	write("</nite:root>", file=outfile, append=T)

}

#add.word.alignment <- function(trans.words, align.words) {
#	trans.align <- data.table(trans.words, wstart=NA, wend=NA)
#	j <- 1
#	for (i in 1:nrow(trans.align)) {
#		if (trans.align$word[i] == align.words$word[j]) {
#			trans.align$wstart[i] <- align.words$starttime[j]
#			trans.align$wend[i] <- align.words$endtime[j]
#			j <- j+1
#		} 
#	}
#
#	return(trans.align)
#} 


## Associate time aligned words in manually determined segments
## xsegs is a datatable holding segment ids, start and end times etc.
## xwords are the new words. 
join.words <- function(xsegs, xwords, xtrans) {
	if (!is.data.table(xwords)) { xwords <- data.table(xwords) }
	if (!is.data.table(xsegs)) { xsegs <- data.table(xsegs) }

	xwords$word <- tolower(xwords$word)

	## unique segments
	usegs <- unique(xsegs) 	
	usegs <- usegs[order(starttime)]	

	## get word overlap with segment intervals
	segs.intervals <- Intervals(usegs[,list(starttime, endtime)])
	word.intervals <- Intervals(xwords[,list(starttime, endtime)])

	word.overlap <- interval_overlap(segs.intervals, word.intervals)
	names(word.overlap) <- usegs$niteid

	## add seg ids
	segs.words <- data.table(ldply(word.overlap, function(x, words) {
		return(words[x])
	}, words=xwords))	
	setnames(segs.words, c(".id", "starttime", "endtime"), c("niteid", "wstart", "wend"))

	## Add segment information back in
	setkey(segs.words, niteid, longconv)
	setkey(usegs, niteid, longconv) 
	segs.words <- usegs[segs.words]
	setnames(segs.words, c("niteid"), c("seg.id"))
	segs.words <- segs.words[order(wstart)]

	if (nrow(segs.words)==0) { return(NULL)	}

	## Add word ids
	segs.words <- data.table(segs.words, niteid=segs.words[,paste(conv, spk, "alignword", 1:nrow(segs.words), sep=".")])

	## Mid utterance speaker changes
	if (nrow(usegs) > nrow(unique(segs.intervals))) {
		## get overlapping segments
		closed(segs.intervals) <- FALSE	
		ovl.ids <- unique(unlist(lapply(interval_overlap(segs.intervals, segs.intervals), function(x) {
			if (length(x) > 1){
				print(x)
				return(x)
			} else {
				return(NULL)
			}
		})))
		#print(ovl.ids)

		usegs <- usegs[order(starttime)]	
		## Remove extra words from other speakers for the turn
		for (ovl in usegs$niteid[ovl.ids]) {
		#	print(ovl)
			spk.words <- toupper(clean.stem.word(xtrans[seg.id == ovl]$word))
			a.words <- toupper(clean.stem.word(segs.words[seg.id == ovl]$word))
			extra.words <- setdiff(a.words, spk.words)		
			print(extra.words)
			segs.words$word[segs.words$seg.id == ovl & toupper(clean.stem.word(segs.words$word)) %in% extra.words] <- ""
		}
	}
	segs.words <- segs.words[word != ""]
	return(segs.words)

} 


get.alignwords.from.ted <- function(filename, transdir="~/data/ted/stm-ted2013/", infofile="~/data/ted/info/id_dname_year_sname", 
	uttdir="~/data/ted/derived/manutt/", 
	worddir="~/data/ted/derived/alignword/",
	vidsrc="ted" )
	 
{
	print("=== get info file ===")
	info.dt <- data.table(read.table(infofile, header=F, fill=T))
        setnames(info.dt, c("id","title","year","filestem"))
        info.dt <- data.table(info.dt, wav.file=paste(info.dt$filestem, "wav", sep="."), video.file=paste(info.dt$filestem,"mp4",sep="."))

	print("=== get stm trans ===")
	print(filename)
	words.dt  <- get.stm.trans(transdir, filename, info.dt, corpus="ted") 

	print("=== get utts ===")
	utts <- unique(words.dt[,list(conv, niteid=seg.id, spk, starttime, endtime, longconv, wav.file, video.file)])
	utts <- data.table(utts, participant=utts[,unlist(lapply(strsplit(unlevel(longconv), split="_"), function(x) {x[1]}))])
	write.conv.seg(utts, dirname=uttdir, segname="manutt")


	print("=== write spurts for prosody processing ===")
        spurts <- utts[,{
                sid <- unlist(lapply(strsplit(niteid, split="\\."), function(x) {x[4]}))
                list(conv=conv, spk=spk, participant=participant, sid=sid, chno=NA, vidsrc=vidsrc,
                starttime=starttime, starttime=endtime, niteid=niteid,
                longconv=longconv, wav.file=wav.file, video.file=video.file
                )}]
        write.features.by.conv(spurts, dirname=uttdir, fsuffix=".manspurts", plain.txt=T)

	print("=== join in aligned words ===")

	alignwords <- read.ctm.words()
	currwords <- alignwords[conv==unique(utts$longconv)]
	setnames(currwords, c("conv"),c("longconv"))	

	alignword.utts <- join.words(utts, currwords, words.dt)		
	alignword.utts <- data.table(alignword.utts, clean.word=alignword.utts[,clean.stem.word(unlevel(word), stem=T, remove.morph=T)])

	if (!is.null(alignword.utts)) {
		currconv <- unique(alignword.utts$conv)
		write.conv.seg(alignword.utts, worddir, "alignword")
		write.table(alignword.utts, paste(worddir, currconv, ".raw.alignword.txt", sep=""))
	}
	return(alignword.utts)

}




