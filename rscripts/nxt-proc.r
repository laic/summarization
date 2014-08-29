## nxt-proc.r: functions for processing NXT format data and
## writing out data as flat files.
## (but some other stuff has crept in it seems)

source("../rscripts/f0basics.r")
library(data.table)
library(intervals)
library(XML)
library(ggplot2)


#--------------------------------------------------------------------------
# Data cleaning
clean.stem.word <- function(xword, stem=F, remove.morph=F) {
	xword[grepl("[{}]", xword)] <- ""
        xword <- gsub("\n", "", xword)
        xword <- gsub("[:@?!^+{}]", "", xword)
        xword <- gsub("[\"=\\.,/`-]", "", xword)
        xword <- gsub("^'$", "", xword)
        xword <- gsub("_", "", xword)
	## Keep transcribed all caps as all caps, for acronyms e.g. (HMM) 
	xword[grep("[a-z]", xword)] <- tolower(xword[grep("[a-z]", xword)])	

        xword <- gsub("^'kay", "okay", xword)
        xword <- gsub("^'cause", "because", xword)
        xword <- gsub("^'ve", "have", xword)
        xword <- gsub("^'ll", "will", xword)
        xword <- gsub("^'fraid", "afraid", xword)
        xword <- gsub("^'til", "until", xword)
        xword <- gsub("^'round", "around", xword)
        xword <- gsub("^['-]", "", xword)
        xword <- gsub("['-;)]$", "", xword)
        xword <- gsub("[-]$", "", xword)
        xword <- gsub("^[-]", "", xword)
        xword <- gsub("^OK$", "okay", xword)
        xword <- gsub("^ok$", "okay", xword)
        xword <- gsub("^hmmm$", "hmm", xword)
        xword <- gsub("^hmmmm$", "hmm", xword)
        xword <- gsub("^mmkay$", "okay", xword)

	if (remove.morph) {
		xword <- gsub("[']s$", "", xword)
		xword <- gsub("[']l$", "", xword)
		xword <- gsub("[']ll$", "", xword)
		xword <- gsub("'v$", "", xword)
		xword <- gsub("'ve$", "", xword)
		xword <- gsub("'d$", "", xword)
		xword <- gsub("'r$", "", xword)
		xword <- gsub("'m$", "", xword)
		xword <- gsub("'re$", "", xword)
		xword <- gsub("'$", "", xword)
	}

	if (stem) {
        	xword <- stemDocument(xword)
	}

       return(xword)

}


#--------------------------------------------------------------------------
# Spurtify

## Segment words into spurts
get.spurts.from.words <- function(nxtwords.dt0) {
        nxtwords.list <- dlply(nxtwords.dt0[.id == "w"], .(fname), function(x){data.table(x)})
	print(nxtwords.list[[1]])
        nxtsegs <- reduce_to_segs_all(nxtwords.list)
	print("here")
        nxtsegs.dt <- data.table(ldply(nxtsegs, function(x) {
                        currconv <- strsplit(unique(x$id), split="\\.")[[1]][1]
                        currspk <- strsplit(unique(x$id), split="\\.")[[1]][2]
                        data.table(conv=currconv, spk=currspk, x)
                }))
        return(nxtsegs.dt)
}


reduce_to_segs_all <- function(y, m.spks=NULL, silthresh=0.25) {
        lapply(y, function(x) {reduce_to_segs(x,m.spks,silthresh)})
}

reduce_to_segs <- function(x, m.spks=NULL, silthresh=0.25, id.name="fname") {
        if (is.null(x)) {
                return(NULL)
        }
	if (!is.data.table(x)) {
		x <- data.table(x)
	}
	setnames(x, c(id.name), c("id")) 

        if (nrow(x[!is.na(starttime)])==0) {
                return(NULL)
        }

        #print(nrow(x[!is.na(starttime)]))
        u <- Intervals(x[!is.na(endtime) & !(starttime > endtime),list(starttime-silthresh, endtime+silthresh)])
        v <- data.table(id=x$id[1], interval_intersection(u))
        setnames(v, c("V1","V2"), c("starttime","endtime"))
        v$starttime <- v$starttime+silthresh
        v$endtime <- v$endtime-silthresh
	
	setnames(x, c("id"), c(id.name)) 
        setkey(v, id)

        if (!is.null(m.spks)) {
		setnames(m.spks, c(id.name), c("id"))	
                setkey(m.spks, id)
		v <- m.spks[v]
		setnames(m.spks, c("id"),c(id.name))	
        } 
	print(v[1])
	return(v)

}

#--------------------------------------------------------------------------
# A wrapper function for applying function fx to windows, or created moving
# windows if windows==NULL.

get.xint.windows <- function(x.list0, wsize=15, tstep=5, windows=NULL, fx=get.xint.window, wkey="xid", ...) {

	## Make sure we're dealing with a data.table	
        if (!is.data.table(x.list0)) {
                x.list <- data.table(x.list0)
        } else {
                x.list <- x.list0
        }

	## Get the current conversation
        currconv <- x.list$conv[1]
        if (is.factor(x.list$conv)) {
                currconv <- unlevel(currconv)
        }
	print("***")
        print(currconv)
        print(wkey)
	print("***")

        if (is.null(windows)) {
		## Make some windows
                print("NO WINDOWS")
                if ("maxtime" %in% names(x.list)) {
                        maxt <- max(x.list$maxtime)
                } else {
                        maxt <- max(x.list$endtime)
                }
                wstarts <- seq(0,maxt,by=tstep)
                wends <- wstarts + wsize
                wints <- data.table(wstarts=wstarts, wends=wends)
        } else {
		## Use given windows, 
                print("WKEY")
                print(unique(x.list[[wkey]]))
                currwkey <- unique(x.list[[wkey]])
                if (currwkey %in% names(windows)) {
                        wints <- windows[[currwkey]]
                } else {
                        return(NULL)
                        return(NULL)
                }
        }
        if ("niteid" %in% names(wints)) {
                print("XXX")
                wx.dt <- data.table(ddply(wints, c("niteid"), fx, xprops=x.list, ...))
                print("HERE")
        } else {
                wx.dt <- data.table(ddply(wints, c("wstarts"), fx, xprops=x.list, ...))
        }

        #print(wx.dt)
        return(wx.dt)
}

get.xint.windows.all <- function(xdt, windows=NULL, wkey="xid") {
        ddply(xdt, c("conv","annot","spk"), get.xint.windows, windows=windows, wkey=wkey, fx=get.xint.window)
}


#------------------------------------------------------------------------------------
## Parse word .xml NXT files
#
nxtwords.to.dt.all  <- function(dirname="~/data/ami/Data/AMI/NXT-format/words/") {
        filenames <- list.files(dirname,pattern="*words.xml")
        #print(filenames)
        return(dlply(data.frame(fn=filenames), .(fn), function(x) {nxtwords.to.dt(unlevel(x$fn), dirname)}))
}

nxtwords.to.dt <- function(fname, dirname="~/data/ami/Data/AMI/NXT-format/words/") {
        print(fname)
        pxml <- paste(dirname, fname,sep="/")
        px <- xmlRoot(xmlTreeParse(pxml, useInternalNodes = TRUE))
        u.part <- xmlToList(px)
        #print(u.part)

        if (names(u.part)[1] == "text") {
                return(NULL)
        }

        if (length(names(u.part)) < 2) {
                print(u.part)
                return(NULL)
        }

        u <- u.part
        m.data <- ldply(u, function(x){
                v <- NULL
                if (!(".attrs" %in% names(x))) {
			xatt <- x
                        v <- data.table(word.id=xatt["id"], starttime=as.numeric(xatt["starttime"]), endtime=as.numeric(xatt["endtime"]), 
				word="")
                } else {
                        xatt <-  x$.attrs
                        v <- data.table(word.id=xatt["id"], starttime=as.numeric(xatt["starttime"]), endtime=as.numeric(xatt["endtime"]), 
				word=x$text)
                }
                return(v)
        } )

        if (!("starttime" %in% names(m.data))) {
                return(NULL)
        }
        return(data.table(fname=fname, m.data))
}

get.das.conv <- function(conv, da.dir="~/data/ami/derived/da/", suffix=".da.txt",
                xnames=c("da.id","starttime","endtime","spk","datype","utt")) {

        filename <- paste(da.dir,"/", conv,  suffix, sep="")
        xlist <- list()
	curr <- data.table(read.delim(filename, header=FALSE))
	setnames(curr, paste("V",1:6,sep=""), xnames)
        xdt <- data.table(conv=conv, curr)
        xdt <- data.table(spk.id=xdt[,paste(conv,spk,sep="-")], xdt[,list(conv,niteid=da.id,spk, starttime,endtime,datype)])
        setkey(xdt, spk.id)
	print("here")
        return(xdt)
}


#------------------------------------------------------------------------------------
get.ami.words.conv <- function(conv="ES2004a", dirname="~/data/ami/Data/AMI/NXT-format/words/") {
        print("get.ami.words.conv")
	filenames <- list.files(dirname,pattern=paste(conv,".*.words.xml", sep=""))
	nxtwords.dt <- NULL
	for (filename in filenames ) {
		currwords <- data.table(nxtwords.to.dt(filename, dirname="~/data/ami/Data/AMI/NXT-format/words/")) 
        	currwords <- get.nxtwords.dt(currwords)
        	currwords <- data.table(currwords, clean.word=clean.stem.word(currwords$word, stem=T, remove.morph=T))
        	setnames(currwords, c("word.id"), c("niteid"))
		nxtwords.dt <- rbindlist(list(nxtwords.dt, currwords))
	}
		
        return(nxtwords.dt)
}
## Read in all NXT format words in the AMI corpus into a data.table
## add cleaned/stemmed version of the word.  
get.ami.words <- function(dirname="~/data/ami/Data/AMI/NXT-format/words/", outfile="./ami.nxtwords.dt0") {
        print("get.ami.words")
        nxtwords <- nxtwords.to.dt.all(dirname=dirname)
        nxtwords.dt <- get.nxtwords.dt(nxtwords)
        nxtwords.dt0 <- data.table(nxtwords.dt, clean.word=clean.stem.word(nxtwords.dt0$word, stem=T, remove.morph=T))
        #nxtwords.dt0 <- clean.nxtwords(nxtwords.dt)
        setnames(nxtwords.dt0, c("id"), c("niteid"))
        save(nxtwords.dt0, file=outfile)
        return(nxtwords.dt0)
}

## Add some things to nxtwords
get.nxtwords.dt <- function(nxtwords) {
	if (is.data.table(nxtwords)) {
		print("dt")
		nxtwords.dt <- nxtwords
	} else if (is.list(nxtwords)) {
        	nxtwords.dt <- data.table(unlist.df(nxtwords))
	}
        nxtwords.dt <- nxtwords.dt[!is.na(starttime)][!is.na(endtime)]
#	print(nxtwords.dt)
        nxtwords.info <- nxtwords.dt[,{x <- strsplit(fname,split="\\.")[[1]]
                                list(conv=x[1],spk=x[2])}
                                ,by=word.id]
        setkey(nxtwords.info, word.id)
        setkey(nxtwords.dt, word.id)
        nxtwords.dt <- nxtwords.info[nxtwords.dt]
        conv.maxtime  <- nxtwords.dt[,list(maxtime=max(endtime)),by=conv]

        setkey(conv.maxtime, conv)
        setkey(nxtwords.dt, conv)
        nxtwords.dt <- conv.maxtime[nxtwords.dt]

        return(nxtwords.dt)

}

clean.nxtwords <- function(nxtwords.dt) {
        nxtwords.clean <- nxtwords.dt[,list(clean.word=clean.stem.word(word)),by=id]
        setkey(nxtwords.clean, id)
        setkey(nxtwords.dt, id)
        nxtwords.dt0 <- nxtwords.dt[nxtwords.clean]
        return(nxtwords.dt0)
}

## Join channel info into a dt, and write as flat file
add.ami.channel.info <- function(nxtsegs.dt, outfile=NULL) {

        m.spks <- get.ami.chan.info()
        chan.info <- m.spks[, list(conv, participant, spk, channel, role)]

        setkey(nxtsegs.dt, conv, spk)
        setkey(chan.info, conv, spk)
        nxtsegs.chan <- chan.info[nxtsegs.dt]
        nxtsegs.chan <- nxtsegs.chan[, list(spk, participant, sid=1:length(spk), channel, starttime, endtime), by=list(conv)]
	if (!is.null(outfile)) {
        	write.table(nxtsegs.chan, file=outfile, quote=F, row.names=F, col.names=F)
	}

        return(nxtsegs.chan)

}

get.ami.chan.info <- function() {
        mxml <- "~/data/ami/Data/AMI/NXT-format/corpusResources/meetings.xml"
        mx <- xmlRoot(xmlTreeParse(mxml, useInternalNodes = TRUE))
        u.meet <- xmlToList(mx)
        u <- u.meet

        m.spks <- ldply(u, function(x){
                v <- NULL
                if (length(x) > 1) {
                        xobs <-  x$.attrs["observation"]
                        spks <- grep("speaker", names(x))
                        xspks <- NULL
                        if (length(spks) > 0) {
				#print(xobs)
				#print(spks)
                                for (i in spks) {
                                        xspks <- rbind(xspks, x[[i]][c("id","channel","nxt_agent","camera",#
                                                "global_name","role") ])
                                }
                        } else {
                                xspks <- "None"
                        }
                        v <- data.table(conv=rep(xobs,nrow(xspks)), xspks)
                }
                return(v)
        } )

        m.spks$.id <- NULL
        m.spks$NA. <- NULL
        #names(m.spks)[2] <- "conv_id"
        #names(m.spks)[6] <- "id"
        m.spks <- data.table(m.spks)
	m.spks$spk <- m.spks$nxt_agent
	m.spks$participant <- m.spks$global_name
        #setkey(m.spks, "id")
        return(m.spks)
}



#----------------------------------------------------------------------------------------
# Read abstract info from query output
get.abs.ext.dt <- function(filename="../ami.abs.ext.all.txt", 
                               abs.names=c("abs.id", "s.id", "slink.id", "niteid", "starttime","endtime","da.type"), corpus=c("ami","icsi")) {
	abs.ext <- data.table(read.table(filename, header=FALSE))
	setnames(abs.ext, abs.names)
	abs.ext <- unique(abs.ext)
#	print(abs.ext)
	if (is.factor(abs.ext$slink.id))	 {
		abs.ext$slink.id <- unlevel(abs.ext$slink.id)
	}
	abs.ext <- data.table(abs.ext[,list(conv=get.conv(niteid),annot=get.slink.annot(slink.id, corpus=corpus)), by=list(slink.id)], abs.ext)
	u <- abs.ext[,list(n.annot=length(unique(annot))), by=conv]
	setkey(u, conv) 
	setkey(abs.ext, conv)

	return(u[abs.ext])
}


#add.linked.eda <- function(abs.ext, group.fx0) {
#       if (!is.data.table(abs.ext)) {
#               abs.ext <- data.table(abs.ext)
#       }
#       u <- abs.ext[, list(n.links=length(slink.id)),by=list(annot, niteid)]
#
#       setkey(u, annot, niteid)
#       setkey(group.fx0, annot, niteid)
#       link.da <- u[group.fx0][!is.na(n.links)][, niteid]
#       xeda <- data.table(group.fx0, link.eda=group.fx0[, niteid %in% link.da])
#
#       return(xeda)    
#
#}


# Extract info from niteids
get.conv <- function(x) {
       if (is.factor(x)) {
               x <- unlevel(x)
       }       
       strsplit(x, split="\\.")[[1]][1] 
}

## summary link annotator
get.slink.annot <- function(x, corpus) {
	#print(x)
       if (is.factor(x)) {
		print("FACTOR")
               	x <- unlevel(x)
       }       
       if (corpus=="ami") {
	#	print("AMI")
               annot <- strsplit(x, split="\\.")[[1]][3] 
       } else if (corpus == "icsi") {
               annot <- strsplit(x, split="\\.")[[1]][2] 
       } else {
               print("unknown corpus")
               print(corpus)
               annot <- "X"
       }

       return (annot)
}


get.mtype <- function(conv, corpus) {
	if (corpus == "ami") {
		mtype <- substr(conv, 7,7)   
	} else if (corpus == "icsi") {
		mtype <- substr(conv, 1,3)
	} else if (corpus == "ted") {
		mtype <- "lec"
	} else {
		mtype <- "x"
	}

	return(mtype)
}

get.mgroup <- function(conv, corpus) {
	if (corpus == "ami") {
		mgroup <- substr(conv, 7,6)   
	} else if (corpus == "icsi") {
		mgroup <- substr(conv, 1,6)
	} else {
		mgroup <- conv
	}

	return(mgroup)
}


## Read in unlinked edas 
add.eda.info.da <- function(x, eda.file="../ami.ext.txt", annot.index=3, corpus) {
        eda.dt <- get.eda.dt(eda.file, annot.index=annot.index)
	print(names(x))
        if("niteid" %in% names(x)) {
                eda.true.dt <- x[, list(niteid=niteid, eda.true=(niteid %in% eda.dt$da.id))]
        } else {
                return(NULL)
        }
 
        setkey(eda.true.dt, niteid)
        setkey(x, niteid)
        eda.da <- x[eda.true.dt]

	 
        eda.dt <- eda.dt[,list(mtype=get.mtype(conv, corpus) , mgroup=get.mgroup(conv, corpus),
                                length(da.id))
                        ,by=list(conv,annot)]
 
        setkey(eda.da, conv)
        setkey(eda.dt, conv)
        return(eda.dt[eda.da])
 
}

get.eda.dt <- function(filename, annot=T, annot.index=2) {
        extsum <- data.table(read.delim(filename, header=FALSE))
        setnames(extsum, c("link.id","da.id","participant","datype","starttime","endtime","X"))
        conv <- unlist(lapply(strsplit(unlevel(extsum$link.id), "\\."), function(x){x[1]}))
        if (annot==T) {
                annot <- unlist(lapply(strsplit(unlevel(extsum$link.id), "\\."), function(x){x[annot.index]}))
                eid <- paste(conv, annot, sep="-")
                extsum <- data.table(eid=eid, conv=conv, annot=annot, extsum)
        } else {
                extsum <- data.table(conv=conv,extsum)
        }

        return(extsum)
}


## Add a field indicating whether a word falls in an EDA zone or not.
project.eda.to.word.all <- function(xeda0, xword0) {
        ## Get list of unique convs and spks.
        conv.spk <- xword0[,length(niteid),by=list(conv,spk)]
        if (is.factor(conv.spk$conv)) {
                conv.spk$conv <- unlevel(conv.spk$conv)
        }
        if (is.factor(conv.spk$spk)) {
                conv.spk$spk <- unlevel(conv.spk$spk)
        }
        eda.dt <- NULL

        # Get link.eda words for each conv and spk.     
        for (i in 1:nrow(conv.spk)) {
                print(conv.spk[i])
                xeda <- xeda0[conv == conv.spk[i]$conv][spk == conv.spk[i]$spk]
                xword <- xword0[conv == conv.spk[i]$conv][spk == conv.spk[i]$spk]
                #print(conv.spk[i]$spk)
                #print(xword[,unique(spk)])

                if (nrow(xword) > 0) {
                        xword.eda <- project.eda.to.word.conv(xeda, xword)
                        eda.dt <- rbind(eda.dt, xword.eda)
                } else {
                        print("No words")
                }
        }

        return(eda.dt)
}

## Find the words in EDA marked time spans
## xeda is a data.table with start and end times of EDAs
## xword is a data.table with start and end times of words. 
project.eda.to.word.conv <- function(xeda, xword) {
        if (nrow(xeda)==0) {
                return(data.table(xword, link.eda=FALSE))
        }
 
        edasegs <- Intervals_full(as.matrix(xeda[,list(starttime, endtime)]))
        wordsegs <- Intervals_full(as.matrix(xword[,list(starttime, endtime)]))
        overlap <- unlist(interval_overlap(edasegs, wordsegs))
 
        return(data.table(xword, link.eda=xword$niteid %in% xword[overlap]$niteid))
}
 
## Get the speaker (nxt_agent) from an AMI da.id (second position)  
get.spk.from.daid <- function(x) {
        if (is.factor(x)) {
                x <- unlevel(x)
        }
 
        spk <- data.table(unlist.df(strsplit(x, split="\\.")))$V2
        return(spk)
}

##--------------------------------------------------------------------------
## Write out feature values in x.dt by conv
write.features.by.conv <-  function(x.dt, dirname, fsuffix, plain.txt=F)  {

        ddply(x.dt, .(conv), function(x) {
                currconv <- unique(x$conv)
                outfile=paste(dirname, "/", currconv, fsuffix, sep="")
                print(outfile)
		if (plain.txt) {
                	write.table(x, file=paste(outfile, ".txt", sep=""), row.names=F, quote=F, col.names=F)
		} else {
			save(x, file=outfile)
			write.table(x, file=paste(outfile, ".txt", sep=""), row.names=F)
		}
                return(outfile)
        })
}
## Write out segs of type segname for a conv 
write.conv.seg <- function(nxtwords.dt0, dirname, segname) {
        dlply(nxtwords.dt0, .(conv), function(x) {
                if (!is.data.table(x)) {
                        x <- data.table(x)
                }
                currconv <- unique(x$conv)

                curr <- x[,list(conv, xid=paste(conv,"-",spk,sep=""), niteid, spk,
                                wstarts=starttime, wends=endtime)]

                x.list <- dlply(curr, .(xid), function(x) {data.table(x)})
                outfile <- paste(dirname, "/", currconv, ".conv.", segname, sep="")
                save(x.list, file=outfile)

                outfile.txt <- paste(outfile, ".txt", sep="")
                print(outfile.txt)
                write.table(curr, file=outfile.txt)

        })
}
## Write out named segments with prespecified size for each conversation.
write.nxtword.windows <- function(x.nxtwords.dt0, wsize=15, wstep=5, dirname="~/data/x/derived/") {

        ## Get the max times
        x.maxtime <- x.nxtwords.dt0[,list(xid=unique(paste(conv,spk,sep="-"))), by=list(conv,spk, maxtime)]

        ## Construct the window sequence and write out
        x.x <- dlply(x.maxtime, .(conv),
        function(y, wsize, wstep) {
                currconv <- unique(y$conv)
                print(currconv)
                x.list <- dlply(y, .(xid), function(x, wsize, wstep) {
                        wstarts <- seq(0,x$maxtime-wsize,by=wstep)
                        wends <- seq(wsize,x$maxtime,by=wstep)
                        niteids <- paste(x$conv, x$spk, "w", wsize, wstarts, sep=".")
                        data.table(conv=x$conv, xid=x$xid,
                                niteid=niteids,
                                wstarts=wstarts,
                                wends=wends)
                }, wsize=wsize, wstep=wstep)
                save(x.list, file=paste(dirname, "/w", wsize, "/", currconv, ".conv.w", wsize, sep=""))
        }, wsize=wsize, wstep=wstep)

}




################################################################################
## Return a matrix of the proportion of time each speaker speaks in windows of
## size tstep through the conv.
## This doesn't really need to be here?
get.spk.props <- function(xsegs, tstep=1, censored=NULL) {
        print(xsegs[1]$id)

	# Initialize matrix
        tx0 <- seq(0,max(xsegs$endtime), by=tstep)
        tx1 <- matrix(rep(0, (nlevels(xsegs$participant)+1) * length(tx0)),
                        ncol=nlevels(xsegs$participant)+1)
        tx <- cbind(tx0, tx1)
        colnames(tx) <- c("time", "silence",levels(xsegs$participant))

	## Get which speaker segments overlap with our intervals of interest
        tsegs <- Intervals_full(as.matrix(data.frame(tx0, tx0+tstep)))
        xints <- Intervals_full(as.matrix(xsegs[,list(starttime, endtime)]))
        xover <- interval_overlap(tsegs, xints)

        if (nrow(tsegs) != length(xover)) {
                print("MISSING INTERVAL")
        }

        for (i in (1:length(xover))) {

		# calculate the amount of silence
                tx[i,"silence"]<- get.silence.size(tsegs[i], xints[xover[[i]]])

                if (length(xover[[i]]) > 0) {
                        for (j in 1:length(xover[[i]])) {
                                curr <- xover[[i]][j]
				## Don't count censored material
                                if (!is.null(censored)) {
                                        if (unlevel(xsegs$id[curr]) %in% unlevel(censored$id)) {
                                                print("!")
                                                vmark <- NA
                                        } else {
                                                z<-interval_intersection(tsegs[i],xints[xover[[i]][j]])
                                                vmark <- size(z)
                                        }
                                } else {
                                        z<-interval_intersection(tsegs[i],xints[xover[[i]][j]])
                                        vmark <- size(z)
                                }
			
				## Add time to the appropriate participant column
                                currtime <- tx[i, unlevel(xsegs$participant[curr])]
                                if (is.na(currtime)) {
                                        tx[i, unlevel(xsegs$participant[curr])] <-  vmark
                                } else {
                                        tx[i,unlevel(xsegs$participant[curr])]<-currtime+vmark
                                }
                        }
                }
                #else {
                #       print(i)
                #}
        }

        return(tx)
}

get.silence.size <- function(tseg, xs) {
        sum(size(interval_intersection(interval_complement(xs), tseg)))
}


