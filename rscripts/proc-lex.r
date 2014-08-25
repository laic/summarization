## Helper functions for calculating lexical features.
source("~/scripts/f0basics.r")
source("~/scripts/nxt-proc.r")
source("~/scripts/ir.r")
library(tm)

## Calculate various term frequency features
collect.tf.feats <- function(x.nxtwords.dt0, word.idf.spk, word.idf.conv, outdir="./", prefix="x") { 

	## tf.idf.spk: each speaker side as a separate document.
	print("tf.idf.spk")
        x.tf.idf <- get.tfidf.spk(x.nxtwords.dt0, word.idf=word.idf.spk)
        #write.table(x.tf.idf, file=paste(dirname,"/", prefix, ".tf.idf.txt")

	print("tf.idf.grp")
        x.tf.idf.grp <- get.tfidf.grp(x.nxtwords.dt0, word.idf=word.idf.conv)
        #write.table(x.tf.idf, file=paste(dirname,"/", prefix, ".tf.idf.grp.txt")

	print("su.idf")
        x.su.idf <- get.su.idf(x.nxtwords.dt0, x.tf.idf)

	## add tf.idf.spk to transcript words 
        setkey(x.nxtwords.dt0, conv, spk, clean.word)
        setkey(x.tf.idf, conv, spk, clean.word)
        x.lex <- x.tf.idf[x.nxtwords.dt0][!is.na(tf)]
        #save(x.lex, file="~/data/ami/derived/x.lex")
        #nrow(x.lex)
		
	## add su.idf
        setkey(x.lex, conv, clean.word)
        setkey(x.su.idf, conv, clean.word)
        x.lex.su <- x.su.idf[x.lex]

	## add tf.idf.grp 
        setkey(x.lex.su, conv, clean.word)
        setkey(x.tf.idf.grp, conv, clean.word)
        x.lex.grp <- x.tf.idf.grp[x.lex.su][!is.na(tf)]
        setnames(x.lex.grp, names(x.lex.grp), gsub(".1$", ".spk", names(x.lex.grp)))

	return(x.lex.grp)

}

### Calculate idf over our collection all.nxtwords.dt0
#get.word.idf <- function() {
#
#	load("~/data/misc-derived/all.nxtwords.dt0")
#        word.idf.spk <- get.idf.spk(all.nxtwords.dt0)
#        write.table(word.idf.spk, file="~/data/misc-derived/all.nxtwords.idf.txt")
#
#        word.idf.conv <- get.idf.conv(all.nxtwords.dt0)
#        write.table(word.idf.conv, file="~/data/misc-derived/all.nxtwords.idf.conv.txt")
#}
#
##--------------------------------------------------------------------------
## Calculate PMI values: x.word.eda is data.table with columns link.eda is T if that 
## DA is a linked EDA.

get.eda.pmi <- function(x.word.eda) {
	N <- nrow(x.word.eda)
	p_t <- nrow(x.word.eda[link.eda==T])/N
	p_f <- nrow(x.word.eda[link.eda==F])/N
	eda.pmi <- x.word.eda[,{
		n.eda <- sum(link.eda==T)
		n.noneda <- sum(link.eda==F)
		n.words <- length(link.eda) 
		list(n.eda=n.eda, n.noneda=n.noneda, 
		n.words=n.words, 
		pmi_t = log((n.eda/n.words)/p_t), 
		pmi_f = log((n.noneda/n.words)/p_f), 
		wfreq = n.words/N
	)},by=list(clean.word)]

	return(eda.pmi)
}

## Fix PMI data, so that missing values are set to zero
## Words that never appear in a categories are set to the observed
## minimum PMI value over the data set.
clean.pmi <- function(x.pmi) {
	x.pmi$n.eda[is.na(x.pmi$n.eda)] <- 0
        x.pmi$n.noneda[is.na(x.pmi$n.noneda)] <- 0
        x.pmi$n.words[is.na(x.pmi$n.words)] <- 0
        x.pmi$pmi_t[is.na(x.pmi$pmi_t)] <- 0
        x.pmi$pmi_f[is.na(x.pmi$pmi_f)] <- 0
        x.pmi$wfreq[is.na(x.pmi$wfreq)] <- 0
        x.pmi$pmi_t[is.infinite(x.pmi$pmi_t)] <- min(x.pmi$pmi_t[!is.infinite(x.pmi$pmi_t)])
        x.pmi$pmi_f[is.infinite(x.pmi$pmi_f)] <- min(x.pmi$pmi_f[!is.infinite(x.pmi$pmi_f)])
	return(x.pmi)

}

##############################################################################################

main.ami.manual.transcripts <- function() {
        ## Get word into one data.table.
        nxtwords.dt0 <- get.ami.words(dirname="~/data/ami/Data/AMI/NXT-format/words/", 
                        outfile="~/data/ami/derived/ami.nxtwords.dt0")

        ## write out timestamps
        write.conv.seg(nxtwords.dt0[.id == "w"], dirname="~/data/ami/derived/word/", segname="word")

        ## Get spurts 
        nxtspurts.dt <- get.spurts.from.words(nxtwords.dt0)

        ## Add channel info 
        spurts.dt <- add.ami.channel.info(nxtspurts.dt, outfile="~/data/ami/derived/ami.spurts.txt")

        spurts.dt <- data.table(read.table("~/data/ami/derived/ami.spurts.txt"))
        setnames(spurts.dt, c("conv","spk","participant","sid","channel","starttime","endtime"))
        spurts.dt <- data.table(niteid=spurts.dt[,paste(conv,spk,participant,sid,sep=".")], spurts.dt)

        write.conv.seg(spurts.dt, dirname="~/data/ami/derived/spurt/", segname="spurt")

}


## A generalized version
main.manual.tf <- function(nxtwords.dt0, lexdir="~/data/ami/derived/lex/", deriveddir="~/data/ami/derived/", 
		prefix="ami", corpus="ami", pattern="[EIT]S") {

 #       load("~/data/ami/derived/ami.asr.nxtwords.dt0")

        nxtwords.dt0$clean.word <- clean.stem.word(nxtwords.dt0$word, stem=T, remove.morph=T)

        ## get tf.idf.spk, su.idf, tf.idf.grp and associated features
        x.lex.grp <- collect.tf.feats(nxtwords.dt0, word.idf.spk=word.idf.spk, word.idf.conv=word.idf.conv)

        write.table(x.lex.grp, file=paste(deriveddir, "/", prefix, ".lex.grp.txt", sep=""))
        write.features.by.conv(x.lex.grp, dirname=lexdir, fsuffix=".lex.grp")

        ## PMI features ####
        x.words <- x.lex.grp[,list(conv, spk, niteid, word, clean.word, starttime, endtime)]

        ## Get EDA times
        abs.names <- c("abs.id", "s.id", "slink.id", "niteid", "starttime","endtime","da.type")
	absfile <- paste(deriveddir, "/", prefix, ".abs.da.txt", sep="")
        x.abs <- get.abs.ext.dt(filename=absfile, corpus=corpus, abs.names=abs.names)
        x.eda <- x.abs[,list(conv, spk=get.spk.from.daid(niteid), niteid, starttime, endtime)]
	
	x.word.eda <- project.eda.to.word.all(x.eda, x.words)
        write.table(x.word.eda, file=paste(deriveddir, "/", prefix, ".word.eda.txt", sep=""))
        write.table(x.word.eda[grep("^[EIT]S", conv)], file=paste(deriveddir, "/", prefix, ".word.eda.txt", sep=""))


        ## Calculate PMI features
        eda.pmi <- get.eda.pmi(x.word.eda[!(conv %in% c(test.convs, dev.convs))])
	
        #eda.pmi <- data.table(eda.pmi, wfreq=eda.pmi[,n.words/sum(eda.pmi$n.words)])

        write.table(eda.pmi, file=paste(deriveddir, "/", prefix, "eda.pmi.txt", sep=""))

        setkey(eda.pmi, clean.word)
        setkey(x.word.eda, clean.word)
        x.pmi <- eda.pmi[x.word.eda][grep(pattern, conv)]

        # Fix words that aren't in the training set.
        x.pmi.clean <- clean.pmi(x.pmi)

        write.table(x.pmi.clean, file=paste(deriveddir, "/", prefix, ".pmi.all.txt", sep=""))

        write.features.by.conv(x.pmi.clean, dirname=lexdir, fsuffix=".pmi")

}

##--------------------------------------------------------------------------
## Parse word .xml NXT files
#nxtwords.to.dt.all  <- function(dirname="~/data/ami/Data/AMI/NXT-format/words/") {
#        filenames <- list.files(dirname,pattern="*words.xml")
#        #print(filenames)
#        return(dlply(data.frame(fn=filenames), .(fn), function(x) {nxtwords.to.dt(unlevel(x$fn), dirname)}))
#}
#nxtwords.to.dt <- function(fname, dirname="~/data/ami/Data/AMI/NXT-format/words/") {
#        print(fname)
#        pxml <- paste(dirname, fname,sep="/")
#        px <- xmlRoot(xmlTreeParse(pxml, useInternalNodes = TRUE))
#        u.part <- xmlToList(px)
#        #print(u.part)
#
#        if (names(u.part)[1] == "text") {
#                return(NULL)
#        }
#
#        if (length(names(u.part)) < 2) {
#                print(u.part)
#                return(NULL)
#        }
#        u <- u.part
#        m.data <- ldply(u, function(x){
#                v <- NULL
#                if (!(".attrs" %in% names(x))) {
#                        xatt <- x
#                        v <- data.table(word.id=xatt["id"], starttime=as.numeric(xatt["starttime"]), endtime=as.numeric(xatt["endtime"]),
#                                word="")
#                } else {
#                        xatt <-  x$.attrs
#                        v <- data.table(word.id=xatt["id"], starttime=as.numeric(xatt["starttime"]), endtime=as.numeric(xatt["endtime"]),
#                                word=x$text)
#                }
#                return(v)
#        } )
#        if (!("starttime" %in% names(m.data))) {
#                return(NULL)
#        }
#        return(data.table(fname=fname, m.data))
#}
#
#
###--------------------------------------------------------------------------
### Read in all NXT format words in the AMI corpus into a data.table
### add cleaned/stemmed version of the word.  
#get.ami.words <- function(dirname="~/data/ami/Data/AMI/NXT-format/words/", outfile="./ami.nxtwords.dt0") {
#	print("get.ami.words")
#	nxtwords <- nxtwords.to.dt.all(dirname=dirname)
#	nxtwords.dt <- get.nxtwords.dt(nxtwords)
#	nxtwords.dt0 <- data.table(nxtwords.dt, clean.word=clean.stem.word(nxtwords.dt0$word, stem=T, remove.morph=T))
#	#nxtwords.dt0 <- clean.nxtwords(nxtwords.dt)
#        setnames(nxtwords.dt0, c("id"), c("niteid"))
#	save(nxtwords.dt0, file=outfile)	
#	return(nxtwords.dt0)
#}
#
#
### Add some things to nxtwords
#get.nxtwords.dt <- function(nxtwords) {
#	nxtwords.dt <- unlist.df(nxtwords)
#	nxtwords.dt <- nxtwords.dt[!is.na(starttime)][!is.na(endtime)] 
#	nxtwords.info <- nxtwords.dt[,{x <- strsplit(fname,split="\\.")[[1]]
#				list(conv=x[1],spk=x[2])}
#				,by=id] 
#	setkey(nxtwords.info, id) 
#	setkey(nxtwords.dt, id)
#	nxtwords.dt <- nxtwords.info[nxtwords.dt]
#	conv.maxtime  <- nxtwords.dt[,list(maxtime=max(endtime)),by=conv]
#
#	setkey(conv.maxtime, conv)
#	setkey(nxtwords.dt, conv)
#	nxtwords.dt <- conv.maxtime[nxtwords.dt]
#
#	return(nxtwords.dt)
#
#}
#
#clean.nxtwords <- function(nxtwords.dt) {
#	nxtwords.clean <- nxtwords.dt[,list(clean.word=clean.stem.word(word)),by=id]
#	setkey(nxtwords.clean, id)
#	setkey(nxtwords.dt, id) 
#	nxtwords.dt0 <- nxtwords.dt[nxtwords.clean]
#	return(nxtwords.dt0)
#}
#

##--------------------------------------------------------------------------

### Add a field indicating whether a word falls in an EDA zone or not.
#project.eda.to.word.all <- function(xeda0, xword0) {
#	## Get list of unique convs and spks.
#	conv.spk <- xword0[,length(niteid),by=list(conv,spk)]
#	if (is.factor(conv.spk$conv)) {
#		conv.spk$conv <- unlevel(conv.spk$conv)
#	} 
#	if (is.factor(conv.spk$spk)) {
#		conv.spk$spk <- unlevel(conv.spk$spk)
#	}
#	eda.dt <- NULL
#
#	# Get link.eda words for each conv and spk.	
#	for (i in 1:nrow(conv.spk)) {	
#		print(conv.spk[i])
#		xeda <- xeda0[conv == conv.spk[i]$conv][spk == conv.spk[i]$spk]	
#		xword <- xword0[conv == conv.spk[i]$conv][spk == conv.spk[i]$spk]	
#		#print(conv.spk[i]$spk)
#		#print(xword[,unique(spk)])
#
#		if (nrow(xword) > 0) {
#			xword.eda <- project.eda.to.word.conv(xeda, xword)
#			eda.dt <- rbind(eda.dt, xword.eda)  
#		} else {
#			print("No words")
#		}
#	}
#
#	return(eda.dt)
#
#}

### Find the words in EDA marked time spans
### xeda is a data.table with start and end times of EDAs
### xword is a data.table with start and end times of words. 
#project.eda.to.word.conv <- function(xeda, xword) {
#	if (nrow(xeda)==0) {
#		return(data.table(xword, link.eda=FALSE))
#	}
#
#	edasegs <- Intervals_full(as.matrix(xeda[,list(starttime, endtime)]))
#	wordsegs <- Intervals_full(as.matrix(xword[,list(starttime, endtime)]))
#	overlap <- unlist(interval_overlap(edasegs, wordsegs))
#
#	return(data.table(xword, link.eda=xword$niteid %in% xword[overlap]$niteid))
#}
#
### Get the speaker (nxt_agent) from an AMI da.id (second position)  
#get.spk.from.daid <- function(x) {
#	if (is.factor(x)) {
#		x <- unlevel(x)
#	}
#
#	spk <- data.table(unlist.df(strsplit(x, split="\\.")))$V2
#	return(spk)
#}
#



### Segment words into spurts
#get.spurts.from.words <- function(nxtwords.dt0) {
#	nxtwords.list <- dlply(nxtwords.dt0[.id == "w"], .(fname), function(x){data.table(x)})
#        nxtsegs <- reduce_to_segs_all(nxtwords.list)
#        nxtsegs.dt <- data.table(ldply(nxtsegs, function(x) {
#                        currconv <- strsplit(unique(x$id), split="\\.")[[1]][1]
#                        currspk <- strsplit(unique(x$id), split="\\.")[[1]][2]
#                        data.table(conv=currconv, spk=currspk, x)
#                }))
#	return(nxtsegs.dt)
#}
#
### Parse the meeting.xml file into flat format
#get.ami.chan.info <- function() {
#	mxml <- "~/data/ami/Data/AMI/NXT-format/corpusResources/meetings.xml"
#	mx <- xmlRoot(xmlTreeParse(mxml, useInternalNodes = TRUE))
#	u.meet <- xmlToList(mx)
#	u <- u.meet
#
#	m.spks <- ldply(u, function(x){
#		v <- NULL
#		if (length(x) > 1) {
#			xobs <-  x$.attrs["observation"]
#			spks <- grep("speaker", names(x))
#			xspks <- NULL
#			if (length(spks) > 0) {
#			print(xobs)
#			print(spks)
#				for (i in spks) {
#					xspks <- rbind(xspks, x[[i]][c("id","channel","spk","camera",#
#						"participant","role") ])
#				}
#			} else {
#				xspks <- "None"
#			}
#			v <- data.frame(conv=rep(xobs,nrow(xspks)), xspks)
#		}
#		return(v)
#	} )
#
#	m.spks$.id <- NULL
#	m.spks$NA. <- NULL
#	#names(m.spks)[2] <- "conv_id"
#	#names(m.spks)[6] <- "id"
#	m.spks <- data.table(m.spks)
#	#setkey(m.spks, "id")
#	return(m.spks)
#}
#
### Join channel info into a dt, and write as flat file
#add.ami.channel.info <- function(nxtsegs.dt, outfile="spurts.txt") { 
#
#	m.spks <- get.ami.chan.info() 
#	chan.info <- m.spks[, list(conv, participant, spk, channel, role)]
#
#        setkey(nxtsegs.dt, conv, spk)
#        setkey(chan.info, conv, spk)
#        nxtsegs.chan <- chan.info[nxtsegs.dt]
#        nxtsegs.chan <- nxtsegs.chan[, list(spk, participant, sid=1:length(spk), channel, starttime, endtime), by=list(conv)]
#        write.table(nxtsegs.chan, file=outfile, quote=F, row.names=F, col.names=F)
#
#	return(nxtsegs.chan)
#
#}


###--------------------------------------------------------------------------
### Write out feature values in x.dt by conv
#write.features.by.conv <-  function(x.dt, dirname, fsuffix)  {
#
#	ddply(x.dt, .(conv), function(x) { 		
#		currconv <- unique(x$conv)
#		outfile=paste(dirname, "/", currconv, fsuffix, sep="")
#		print(outfile)
#		save(x, file=outfile)
#		write.table(x, file=paste(outfile, ".txt", sep=""), row.names=F)
#		return(outfile)
#	})
#}
### Write out segs of type segname for a conv 
#write.conv.seg <- function(nxtwords.dt0, dirname, segname) { 
#        dlply(nxtwords.dt0, .(conv), function(x) {
#		if (!is.data.table(x)) {
#                	x <- data.table(x)
#		}
#                currconv <- unique(x$conv)
#		
#                curr <- x[,list(conv, xid=paste(conv,"-",spk,sep=""), niteid, spk, 
#                                wstarts=starttime, wends=endtime)]
#
#		x.list <- dlply(curr, .(xid), function(x) {data.table(x)})  
#                outfile <- paste(dirname, "/", currconv, ".conv.", segname, sep="")
#                save(x.list, file=outfile)
#
#                outfile.txt <- paste(outfile, ".txt", sep="")
#		print(outfile.txt)
#                write.table(curr, file=outfile.txt)
#
#        })
#}
#
#
### Write out named segments with prespecified size for each conversation.
#write.ami.windows <- function(ami.nxtwords.dt0, wsize=15, wstep=5, dirname="~/data/ami/derived/") {
#
#	## Get the max times
#	ami.maxtime <- ami.nxtwords.dt0[,list(xid=unique(paste(conv,spk,sep="-"))), by=list(conv,spk, maxtime)]	
#
#	## Construct the window sequence and write out
#	ami.x <- dlply(ami.maxtime, .(conv), 
#	function(y, wsize, wstep) {
#		currconv <- unique(y$conv)
#		print(currconv)
#		x.list <- dlply(y, .(xid), function(x, wsize, wstep) {
#			wstarts <- seq(0,x$maxtime-wsize,by=wstep) 
#			wends <- seq(wsize,x$maxtime,by=wstep)
#			niteids <- paste(x$conv, x$spk, "w", wsize, wstarts, sep=".")    
#			data.table(conv=x$conv, xid=x$xid, 
#				niteid=niteids,
#				wstarts=wstarts, 
#				wends=wends)
#		}, wsize=wsize, wstep=wstep)
#		save(x.list, file=paste(dirname, "/w", wsize, "/", currconv, ".conv.w", wsize, sep=""))
#	}, wsize=wsize, wstep=wstep)
#
#}



##############################################################################################



#add.eda.info.da <- function(x, eda.file="../ami.ext.txt", annot.index=3) {
#	eda.dt <- get.eda.dt(eda.file, annot.index=annot.index)
#	if("niteid" %in% names(x)) {	 
#		eda.true.dt <- x[, list(niteid=niteid, eda.true=(niteid %in% eda.dt$da.id), dur=endtime-starttime)]	
#	} else {
#		return(NULL)
#	}
#
#	print("!")	
#	setkey(eda.true.dt, niteid) 
#	setkey(x, niteid)
#	eda.da <- x[eda.true.dt]
#
#	print("!!")
#	eda.dt <- eda.dt[,list(mtype=unique(substr(conv,7,7)), mgroup=unique(substr(conv,1,6)), 
#				length(da.id))
#			,by=list(conv,annot)]
#
#	setkey(eda.da, conv)
#	setkey(eda.dt, conv)
#
#	print("!!!")
#	return(eda.dt[eda.da])
#
#}



#print.lex.info <- function(x, ami.lex.grp){
#	#currconv <- strsplit(unlevel(x$niteid), split="\\.")[[1]][1]	
#	ami.lex.grp[conv == x$conv][starttime < x$endtime][endtime > x$starttime, list(conv,starttime, endtime, word, su.idf, tf.idf, spk)][order(spk, starttime)]
#
#}


#read.lex.pros <- function(prosdir="/disk/scratch/ami/derived/segs/f0", pattern="*.aggs.word$", add.name=F) {
#	x.lex.pros <- NULL
#	filenames <- list.files(prosdir, pattern=pattern)
#	print(filenames)
#	for (filename in filenames) {
#		print(filename)
#		curr <- data.table(read.table(paste(prosdir,filename,sep="/"), header=TRUE))
#		if(add.name) {
#			curr <- data.table(fname=filename, curr) 
#		}
#		x.lex.pros <- rbind(x.lex.pros, curr)
#	}
#
#	return(data.table(x.lex.pros))
#}


#split.window.data <- function(x.list, outdir, mc.cores=4, wkey="conv", suffix="") {
#	u <- mclapply(x.list, function(x) {
#		currconv <- unique(x[[1]][[wkey]]) 	
#		print(currconv)
#		#outfile <- paste(outdir,"/",currconv, suffix, ".txt", sep="")
#		#write.table(x, file=outfile, row.names=F, quote=F) 
#		save(x, file=paste(outdir,"/",currconv, suffix, sep=""))
#	}, mc.cores=mc.cores) 
#
#	return(u)
#}

#split.raw.pros <- function(x.list, outdir, mc.cores=4, wkey="conv", suffix="") {
#	u <- mclapply(x.list, function(x) {
#		currconv <- unique(x[[wkey]]) 	
#		print(currconv)
#		if (!("xid" %in% names(x))) {
#			x <- data.table(xid=x[,paste(conv,nxt_agent, sep="-")], x)
#		}
#		outfile <- paste(outdir,"/",currconv, suffix, ".txt", sep="")
#		write.table(x, file=outfile, row.names=F, quote=F) 
#		save(x, file=paste(outdir,"/",currconv, suffix, sep=""))
#	}, mc.cores=mc.cores) 
#
#	return(u)
#}
#
#get.ami.das <- function(da.dir="~/data/ami/derived/da/", spks.file="../mm/ami.spks") {
#	ami.das <- get.das(da.dir)
#	xspks.obj <- load(spks.file)	
#	ami.spks <- get(xspks.obj)	
#	setnames(ami.spks, c("sid"), c("spk.id"))
#
#	setkey(ami.spks, spk.id, conv)
#	setkey(ami.das, spk.id, conv)
#	ami.das <- ami.spks[ami.das]
#	ami.das <- data.table(xid=ami.das$conv, ami.das)
#
#	return(ami.das)
#}

#get.pscores.da.old <- function(x.das, da.dir="~/data/ami/derived/segs/da/") {
#	x.pscores.da <- NULL
#	da.windows <- x.das[,list(xid=xid, conv, niteid=niteid, wstarts=starttime, wends=endtime)]
#	#da.windows <- x.das
#	da.windows.list <- dlply(da.windows, c("xid"), function(x) {
#			currconv <- unique(x$conv)
#			outfile <- paste(da.dir, "/", currconv, ".full.da.txt", sep="")  
#			write.table(x, file=outfile, row.names=F)
#			return(data.table(x))
#		})
#
#	print("spk.dur")
#	x.spk.dur.da <- get.spk.dur.da(x.das, windows=da.windows.list, xvars=c("conv")) 	
#
#	print("spk.prop")	
#	x.spk.prop.da <- get.spk.prop.da(x.spk.dur.da)	
#	x.spk.prop.da <- x.spk.prop.da[is.finite(spkprop)]
#	x.spk.prop.list <- dlply(x.spk.prop.da, .(conv), function(x) {data.table(x)})
#
#	print("partqual")
#	x.partqual.da <- data.table(unlist.df(mclapply(x.spk.prop.list, get.partqual.sum, 
#				wkey="niteid", window.ext.time=0, mc.cores=4))) 	
#	save(x.partqual.da, file="x.partqual.da")
#
#	print("partqual")
#	x.partqual.da.10 <- data.table(unlist.df(mclapply(x.spk.prop.list, get.partqual.sum,  
#				wkey="niteid", window.ext.time=10, mc.cores=4))) 	
#	setnames(x.partqual.da.10, names(x.partqual.da.10), gsub("$", ".10", names(x.partqual.da.10)))
#	setnames(x.partqual.da.10, c("niteid.10"), c("niteid"))
#	save(x.partqual.da.10, file="x.partqual.da.10")
#
#	setkey(x.partqual.da, niteid)
#	setkey(x.partqual.da.10, niteid)
#	x.partqual.da <- x.partqual.da[x.partqual.da.10]
#
#	x.da.list <- dlply(x.das, .(conv), function(x) {data.table(x)})
#
#	print("ent")
#	ent.tmats <- get.ent.all(x.da.list, windows=da.windows.list)
#	save(ent.tmats, file="x.ent.mats")
#	x.ent.da <- ent.tmats$ent[,list(niteid,H,F,hcond,Fcond)]
#
#	setkey(x.ent.da, niteid)
#	setkey(x.partqual.da, niteid)			
#
#	x.pscores.da <- x.partqual.da[x.ent.da]	
#	setkey(x.pscores.da, niteid)
#
#
#	return(x.pscores.da)
#}

### Used in get-pscores-conv.r?
#get.spk.prop.da <- function(x.spk.dur.da) {
#
#	# meetprop is the group proportion	
#	x.all <- x.spk.dur.da[,{
#			spktime.all <- sum(sum.dur)
#			dur <- endtime-starttime 
#			spkprop <- sum.dur/spktime.all
#			meetprop <- sum.dur/dur
#
#			list(spk=spk.id, dur=dur, 
#				spkprop=meetprop, meetprop=meetprop, 
#				spktime=sum.dur, spktime.all=spktime.all, 
#				mrank=length(meetprop)-rank(meetprop,ties.method="min"),
#				nspks=length(meetprop), 
#				n.da=n.da,
#				n.da.all=sum(n.da)) 	
#		},by=list(niteid, conv)]
#
#	
#	return(x.all)
#}
#
#get.da.ovl <- function(x.das, mc.cores=4) {
#	x.da.list <- dlply(x.das[,list(conv,sid,starttime,endtime,participant)], .(conv), function(x) {data.table(x)})
#	
#	print("latency")
#	x.latency <- mclapply(x.da.list, get.latency, mc.cores=mc.cores)
#	save(x.latency, file="x.latency.list")
#	x.latency <- unlist.df(x.latency)
#	x.latency0 <- x.latency[,list(niteid=sid, stay.from.prevs, stay.to.nexts, time.from.prevs, time.to.nexts)]
#
#	## Barges into previous DAs 	
#	print("barge into")
#	x.barge <- mclapply(x.da.list , get.barge, mc.cores=mc.cores)
#	x.barge.dt <- unlist.df(x.barge)
#	setnames(x.barge.dt, c("sid"), c("niteid")) 
#	save(x.barge.dt, file="x.barge.dt") 
#
#	## Barges onto the current DA
#	print("barge onto")
#	x.barge.onto <- unlist.df(mclapply(x.da.list , get.barge.onto, mc.cores=mc.cores))
#	setnames(x.barge.onto,c("sid"), c("niteid")) 
#	save(x.barge.onto, file="x.barge.onto")
#
#	setkey(x.barge.dt, niteid)
#	setkey(x.latency0, niteid)
#	x.da.ovl <- x.barge.dt[x.latency0]
#
#	setkey(x.barge.onto, niteid, conv, starttime, endtime)
#	setkey(x.da.ovl, niteid, conv, starttime, endtime)
#
#	#setkey(x.barge.onto, niteid, conv, starttime, endtime)
#	#setkey(x.barge.dt, niteid, conv, starttime, endtime)
#
#	x.da.ovl <- x.da.ovl[x.barge.onto]
#
#	return(x.da.ovl)
#}



##-------------------------------------------------------------------------------------
## First pass at weighting tf.idf features
##-------------------------------------------------------------------------------------
#
#weight.tf.scale <- function(tf.idf, prosvar.pos, pros.weights) {
#	tf.pros.add <- NULL	
#	wnames  <- NULL	
#	for (pros.weight in pros.weights) {
#		for (tf.weight in pros.weights) {
#			if (pros.weight != tf.weight) {
#				tf.pros.add <- cbind(tf.pros.add, ((tf.weight * tf.idf) + (pros.weight * prosvar.pos))/(tf.weight+pros.weight))
#				wnames <- c(wnames, paste(tf.weight, pros.weight, sep="-"))
#			}
#		}
#	}
#	tf.pros.add <- data.table(tf.pros.add)
#	setnames(tf.pros.add, wnames)
#
#	return(tf.pros.add) 
#}
#
#
#weight.tf.f0 <- function(tf.idf, su.idf, prosvar, prosvar.pos, pros.weights=c(1:5)) {   
#	tf.pros <- tf.idf 
#	tf.pros[prosvar > 0] <- tf.pros[prosvar > 0] * (1 + prosvar[prosvar > 0]) 	
#
#	tf.pros.0 <- tf.idf 
#	tf.pros.0 <- tf.pros.0 + prosvar.pos	
#
#	tf.pros.add <- weight.tf.scale(tf.idf, prosvar.pos, pros.weights)
#	setnames(tf.pros.add, names(tf.pros.add), gsub("^", "tf.pros.", names(tf.pros.add)))
#
#	su.pros <- su.idf 
#	su.pros[prosvar > 0] <- su.pros[prosvar > 0] * (1 + prosvar[prosvar > 0]) 	
#
#	su.pros.0 <- su.idf 
#	su.pros.0 <- su.pros.0 + prosvar.pos	
#
#	su.pros.add <- weight.tf.scale(su.idf, prosvar.pos, pros.weights)
#	setnames(su.pros.add, names(su.pros.add), gsub("^", "su.pros.", names(su.pros.add)))
#
#	return(data.table(tf.pros=tf.pros, tf.pros.0=tf.pros.0, tf.pros.add, 
#		su.pros=su.pros, su.pros.0=su.pros.0, su.pros.add))
#}
#
#
### Weight tf.idf, su.idf with prosodic features using predetermined weights
#get.tf.pros <- function(x.lex.grp, x.lex.f0.dt, tf.aggs, agg.name="mean.normX0", var.name="X0") {
#	f0.x <- x.lex.grp[x.lex.f0.dt]
#
#	setnames(f0.x, c(agg.name), c("prosvar"))
#
#	f0.x <- f0.x[,list(id, starttime, endtime, tf.idf, su.idf, 
#			tf.scaled=tf.idf/tf.aggs$sd.tf, 
#			su.scaled=su.idf/tf.aggs$sd.su, 
#			prosvar, prosvar.pos=prosvar + abs(min(prosvar, na.rm=T)))
#		,by=list(conv,participant)]
#
#	f0.x$prosvar[is.na(f0.x$prosvar)] <- 0
#	f0.x$prosvar.pos[is.na(f0.x$prosvar.pos)] <- 0
#
#	#f0.y <- f0.x[,{ u <- weight.tf.f0(tf.idf, su.idf, prosvar, prosvar.pos)
#	f0.y <- f0.x[,{ u <- weight.tf.f0(tf.scaled, su.scaled, prosvar, prosvar.pos)
#			#print(u)
#			data.table(id=id, u)}] 
#
#
#	setnames(f0.y, names(f0.y), gsub("pros", agg.name, names(f0.y)))
#	setnames(f0.x, names(f0.x), gsub("prosvar", agg.name, names(f0.x)))
#	setkey(f0.x, id)	
#	setkey(f0.y, id)
#
#	return(f0.x[f0.y])
#}

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------

#get.lex.f0 <- function(ami.lex, ami.f0.st) {
#	word.windows <- ami.lex[corpora=="ami",list(conv, spk, xid=paste(conv,spk,sep="-"), niteid=id, wstarts=starttime, wends=endtime)]
#	word.windows.list <- dlply(word.windows, c("conv"), function(x) {dlply(x, c("xid"),function(y){y})})
#
#	ami.f0.st <- lapply(ami.f0.st, function(x) {data.table(xid=x[,paste(conv,nxt_agent,sep="-")], x)})
#
#	ami.lex.f0 <- get.f0.aggs.all(ami.f0.st, windows=word.windows.list, wkey="xid")
#	save(ami.lex.f0, file="ami.lex.f0")
#	ami.lex.f0.dt <- unlist.df(ami.lex.f0)
#
#	return(ami.lex.f0.dt)
#
#
#	
#}  

#get.lex.i0 <- function(word.windows.list, ami.i0.zs) {
##	word.windows <- ami.lex[,list(xid=paste(conv,spk,sep="-"), niteid=id, wstarts=starttime, wends=endtime)]
##	word.windows.list <- dlply(word.windows, c("xid"), function(x) {x})	
##	ami.i0.zs <- lapply(ami.i0.zs, function(x) {data.table(xid=x[,paste(conv,nxt_agent,sep="-")], x)})
#
#	ami.lex.i0 <- get.i0.aggs.all(ami.i0.zs, windows=word.windows.list, wkey="xid")
#	save(ami.lex.i0, file="ami.lex.i0")
#	ami.lex.i0.dt <- unlist.df(ami.lex.i0)
#
#	return(ami.lex.i0.dt)
#
#
#}


#get.eda.mods <- function(ami.tfpros, ami.if0.sph.eda) {
#	group.fx <- ami.tfpros.eda
#	f0.grp <- ami.if0.sph.eda[,list(mean.F0.grp=mean(mean.normF0, na.rm=T), 
#		mean.I0.grp=mean(mean.normI0, na.rm=T)
#		),by=list(wid)]
#	setkey(f0.grp, wid)
#	setkey(group.fx, wid)
#
#	group.fx <- f0.grp[group.fx]
#	group.fx <- group.fx[!is.na(mean.F0.grp)]
#	group.fx <- group.fx[!is.na(mean.I0.grp)]
#
#	m.rate.i0.high <- mod.edag.rate.i0.high(group.fx); display(m.rate.i0.high)
#	cv.rate.i0.high <- get.cv.wrapper(m.rate.i0.high, xdata=group.fx, nfolds=10, N="rate.i0.high", spk=F)
#
#	m.rate.f0.high <- mod.edag.rate.f0.high(group.fx); display(m.rate.f0.high)
#	cv.rate.f0.high <- get.cv.wrapper(m.rate.f0.high, xdata=group.fx, nfolds=10, N="rate.f0.high", spk=F)
#	
#	m.tfidf <- mod.edag.tfidf(group.fx); display(m.tfidf)
#	cv.tfidf <- get.cv.wrapper(m.tfidf, xdata=group.fx, nfolds=10, N="tfidf", spk=F)
#
#	m.tf.su <- mod.edag.tf.su(group.fx); display(m.tf.su)
#	cv.tf.su <- get.cv.wrapper(m.tf.su, xdata=group.fx, nfolds=10, N="tf.su", spk=F)
#
#	m.tfpros <- mod.edag.tfpros(group.fx); display(m.tfpros)
#	cv.tfpros <- get.cv.wrapper(m.tfpros, xdata=group.fx, nfolds=10, N="tfpros", spk=F)
#
#	m.su.i0 <- mod.edag.su.i0(group.fx); display(m.su.i0)
#	cv.su.i0 <- get.cv.wrapper(m.su.i0, xdata=group.fx, nfolds=10, N="su.i0", spk=F)
#
#	m.su.f0 <- mod.edag.su.f0(group.fx); display(m.su.f0)
#	cv.su.f0 <- get.cv.wrapper(m.su.f0, xdata=group.fx, nfolds=10, N="su.f0", spk=F)
#
#	m.tf.i0 <- mod.edag.tf.i0(group.fx); display(m.tf.i0)
#	cv.tf.i0 <- get.cv.wrapper(m.tf.i0, xdata=group.fx, nfolds=10, N="tf.i0", spk=F)
#
#	m.tf.f0 <- mod.edag.tf.f0(group.fx); display(m.tf.f0)
#	cv.tf.f0 <- get.cv.wrapper(m.tf.f0, xdata=group.fx, nfolds=10, N="tf.f0", spk=F)
#
#
#	m.tfidf.grp <- mod.edag.tfidf.grp(group.fx); display(m.tfidf.grp)
#	cv.tfidf.grp <- get.cv.wrapper(m.tfidf, xdata=group.fx, nfolds=10, N="tfidf", spk=F)
#
#	m.suidf <- mod.edag.suidf(group.fx); display(m.suidf)
#	cv.suidf <- get.cv.wrapper(m.suidf, xdata=group.fx, nfolds=10, N="suidf", spk=F)
#
#	m.sph <- mod.edag.sph(group.fx)
#	cv.sph <- get.cv.wrapper(m.sph, xdata=group.fx, nfolds=10, N="sph", spk=F)
#
#	m.f0 <- mod.edag.f0(group.fx)
#	cv.f0 <- get.cv.wrapper(m.f0, xdata=group.fx, nfolds=10, N="f0", spk=F)
#
#	m.su.i0.sph <- mod.edag.su.i0.sph(group.fx); display(m.su.i0.sph)
#	cv.su.i0.sph <- get.cv.wrapper(m.su.i0.sph, xdata=group.fx, nfolds=10, N="sph", spk=F)
#
#	m.tfidf.sph <- mod.edag.tfidf.sph(group.fx); display(m.tfidf.sph)
#	cv.tfidf.sph <- get.cv.wrapper(m.tfidf.sph, xdata=group.fx, nfolds=10, N="sph", spk=F)
#
#	return(list(cv.tfidf, cv.sph, cv.tfidf.sph))	
#}


#get.tcorpus <- function(xword.docs, has.spk=F) {
#	xcorpus <- Corpus(VectorSource(xword.docs[,text]))
#	if (has.spk) {
#		names(xcorpus) <- xword.docs[,paste(conv,spk, sep="-")]
#	} else {
#		names(xcorpus) <- xword.docs[,paste(conv,sep="-")]
#	}
#	print(xcorpus)	
#	xcorpus <- clean.docs(xcorpus)
#
#
#	return(xcorpus)
#
#}

#get.dtm.idf <- function(tcorpus) {
#
#	dtm.idf <- TermDocumentMatrix(tcorpus[1:3])
#
#
#	dtm.idf <- DocumentTermMatrix(tcorpus, control=list(weighting=function(x){weightTfIdf(x, normalize=T)}, 
#			stopwords=TRUE, 
#			removeNumbers=T
#			))
#	dimnames(dtm.idf)[[1]] <- names(tcorpus)
#	return(dtm.idf)
#}


#get.tf.idf.dt <- function(dtm.idf) {
#
#	dtm.idf.mat <- as.matrix(dtm.idf)
#	x <- data.frame(sides=rownames(dtm.idf.mat), dtm.idf.mat)
#	names(x) <- c("sides", colnames(dtm.idf.mat))
#
#	dtm.idf.dt <- data.table(melt(x))
#
#	u <- dtm.idf.dt[,{x <- strsplit(unlevel(sides),split="-")[[1]]; list(conv=x[1], spk=x[2])},by=sides]
#	setkey(u, sides)
#	setkey(dtm.idf.dt, sides)
#	dtm.idf.dt <- u[dtm.idf.dt]
#	setnames(dtm.idf.dt, c("variable"), c("clean.word"))
#	
#	return(dtm.idf.dt) 
#}
#

#add.eda.info <- function(ami.tf.idf, ami.if0.sph.eda) {
#
#	setkey(ami.if0.sph.eda, wid)
#	setkey(ami.tf.idf, wid)
#	ami.if0.lex <- ami.tf.idf[ami.if0.sph.eda]
#
#	group.fx <- unique(ami.if0.lex[,list(wid,corpus,conv=conv.1,meeting,mgroup,mtype,annot,
#		Fcond,partqual,bargein.rate,n.vsu,total.ovl,sil.prop, 
#		sum.tf.idf=to.zscore(sum.tf.idf), sum.idf=to.zscore(sum.idf), 
#		sum.tf.idf.grp=to.zscore(sum.tf.idf.grp), sum.idf.grp=to.zscore(sum.idf.grp), 
#		sum.su.idf=to.zscore(sum.su.idf), sum.ave.surp=to.zscore(sum.ave.surp), 	
#		sum.su.F0=to.zscore(sum.su.F0), sum.tf.F0=to.zscore(sum.tf.F0), 
#		sum.su.I0=to.zscore(sum.su.I0), sum.tf.I0=to.zscore(sum.tf.I0), 
#		mean.word.F0=to.zscore(mean.word.F0),
#		mean.word.I0=to.zscore(mean.word.I0),
#		n.F0.high=to.zscore(n.F0.high), n.I0.high=to.zscore(n.I0.high),
#		rate.F0.high=to.zscore(rate.F0.high), rate.I0.high=to.zscore(rate.I0.high),
#		eda.time=eda.total.time, eda.true=eda.total.time>0, eda.true.half=eda.total.time > 7.5)]) 	
#
#	group.fx <- group.fx[!is.na(Fcond)]
#
#	return(group.fx)
#
#} 

## LDA example
#get.lda.mods <- function(tcorpus) {
#	dtm <- DocumentTermMatrix(tcorpus,
#                control = list(stopwords = TRUE, minWordLength = 1,
#                removeNumbers = TRUE))
#	dimnames(dtm)[[1]] <- names(tcorpus)
#
#	rowx <- apply(dtm, 1, sum)
#	dtm <- dtm[rowx > 0]
#	save(dtm, file="ami.dtm")
#
#	dtm.lda <- get.lda.models(dtm)
#
#	return(dtm.lda)
#}


