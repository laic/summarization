source("~/scripts/nxt-proc.r")
library(data.table)
library(tm)

##---------------------------------------------------------------------------
## Term frequency features
##---------------------------------------------------------------------------
## get.surp: for each unique word, spk, conv.  
## surp(s,t) = -log( [Sum_{s'!=s} TF(t,s')] / [Sum_{r'=!s}spk.word.count(r)] )
## Use tf.idf from get.tfidf.spk  

get.surp <- function(xword.dt0, x.tf.idf) {
	## Count number of words from each speaker, for each conv
        spk.word.count <- xword.dt0[clean.word != "",list(wcount=length(clean.word), n.spks=unique(n.spks)),by=list(conv,spk)]

        surp.dt <- NULL
        for (i in 1:nrow(spk.word.count)) {
                currspk <- spk.word.count[i, spk]
                currconv <- spk.word.count[i, conv]

		## Sum total words of other participants 
                other.wcount <- spk.word.count[conv==currconv][spk != currspk, sum(wcount)]

		## Just to check we're actually in a dialogue
                n.spks <- spk.word.count[conv==currconv][spk != currspk, unique(n.spks)]
		if (length(n.spks) > 0) {
			## Stats on tf of other people for each word spoken by other people 
			other.tf <- x.tf.idf[conv==currconv][spk != currspk,list(conv=currconv, spk=currspk,
					sum.tf=sum(tf),
					surp=-log(sum(tf)/other.wcount),
					surp.tcount=-log(sum(tcount)/other.wcount),
					other.wcount=other.wcount,
					n.spks=n.spks),list(clean.word)]
		} else {
			## We're in a lecture or other such monologue
			other.tf <- x.tf.idf[conv==currconv][,list(conv=currconv, spk=currspk,
					sum.tf=sum(tf),
					surp=0, #-log(sum(tf)/other.wcount),
					surp.tcount=0, #-log(sum(tcount)/other.wcount),
					other.wcount=0,
					n.spks=1),list(clean.word)]

		}
                surp.dt <- rbindlist(list(surp.dt, other.tf))

        }

        return(surp.dt)

}

## get.ave.surp: average the surprisal scores from get.surp 
## If only one person, s, says the word, the surp(t,s) is technically infinite!
## At the moment we are just discarding these cases, so we basically just get
## the suprise of the other participants.  If we put an arbitrarily large 
## number in for this, we would just get swamped by single occurences. 
 
get.ave.surp <- function(surp.dt) {
        return(surp.dt[,list(ave.surp=sum(surp)/max(n.spks),
                        ave.surp.tcount=sum(surp.tcount)/max(n.spks)
			),
                by=list(conv, clean.word)])
}



## get.su.idf: use speaker dependent tf information, i.e. from get.tfidf.spk. 
get.su.idf <- function(xwords.dt0, x.tf.idf) {

	## Add number of speakers for each conv
        N.spks <- xwords.dt0[,list(n.spks=length(unique(spk))),by=list(conv)]
        setkey(N.spks, conv)
        setkey(xwords.dt0, conv)
        xwords <- N.spks[xwords.dt0][clean.word != ""]

        setkey(x.tf.idf, conv)
        x.tf.idf <- N.spks[x.tf.idf]

	## Get surprise values
        surp.dt <- get.surp(xwords, x.tf.idf)
        ave.surp <- get.ave.surp(surp.dt)

	## Proportion of speakers who use a particular word. 
        prop.spks.dt <- x.tf.idf[,list(prop.spks=length(spk)/unique(n.spks), idf=unique(idf), n.spks=unique(n.spks)),by=list(conv, clean.word)]
        setkey(prop.spks.dt, conv, clean.word)
        setkey(ave.surp, conv, clean.word)

        su.idf.dt <- prop.spks.dt[ave.surp]
        su.idf.dt <- su.idf.dt[,list(su.idf=ave.surp*prop.spks*sqrt(idf),
                su.idf.tcount=ave.surp.tcount*prop.spks*sqrt(idf),
                ave.surp=ave.surp, prop.spks=prop.spks, sqrt.idf=sqrt(idf), n.spks=n.spks), by=list(conv,clean.word)]

        return(su.idf.dt)

}


update.idf.conv <- function(xwords.dt0, word.idf, spk=F) {
        if (spk) {
                ## treat different speaker sides as different documents
                word.dcount <- xwords.dt0[clean.word != "", list(dcount=length(unique(paste(conv, spk)))),by=list(clean.word)]
                print(unique(xwords.dt0[, paste(conv,spk)]))
                num.docs <-  length(unique(xwords.dt0[, paste(conv,spk)]))
        } else {
                word.dcount <- xwords.dt0[clean.word != "", list(dcount=length(unique(conv))),by=list(clean.word)]
                print(length(unique(xwords.dt0[, paste(conv)])))
                num.docs <-  length(unique(xwords.dt0[, paste(conv)]))
        }
        newD <- unique(word.idf$D) + num.docs

        if (length(newD) > 1) {
                print("non-unique D > 1, exiting")
                return(NULL)
        }

        ## Add counts to word.idf table
        for (i in 1:nrow(word.dcount)) {
                currword <- word.dcount$clean.word[i]
                currdcount <-  word.dcount$dcount[i]
                if (currword %in% word.idf$clean.word)  {
                        word.idf$dcount[word.idf$clean.word == currword] <- word.idf$dcount[word.idf$clean.word == currword] + currdcount
                } else {
                        ## Add a new word
                        word.idf <- rbindlist(list(word.idf,
                                data.table(clean.word=currword, dcount=currdcount, df=currdcount/newD,
                                        idf=log(newD/currdcount), D=newD)))
                }
        }

        word.idf$D <- newD
        word.idf$df <- word.idf[,dcount/D]
        word.idf$idf <- word.idf[,log(D/dcount)]

        return(word.idf)
}

get.idf.conv <- function(xwords.dt0) {
	## doc term counts
        conv.tcount <- xwords.dt0[clean.word != "",list(tcount=length(niteid)),by=list(conv,clean.word)]

	## Number of documents
        D <- nrow(conv.tcount[,length(clean.word),list(conv)])

	## For each term: document count, document frequency, IDF
        word.idf <- conv.tcount[,{
		dcount <- length(conv) 
                list(dcount=dcount, df=dcount/D, idf=log(D/dcount), D=D)
                },by=list(clean.word)]

	return(word.idf)
}


get.idf.spk <- function(xwords.dt0) {
	## doc term counts
        spk.tcount <- xwords.dt0[clean.word != "",list(tcount=length(niteid)),by=list(conv,spk,clean.word)]

	## Number of documents
        D <- nrow(spk.tcount[,length(clean.word),list(conv, spk)])

	## For each term: document count, document frequency, IDF
        word.idf <- spk.tcount[,{
		dcount <- length(conv) 
                list(dcount=dcount, df=dcount/D, idf=log(D/dcount), D=D)
                },by=list(clean.word)]

	return(word.idf)
}


## Get.tfidf.spk: calculate TF.IDF treating different speakers as different documents.
get.tf.spk <- function(xwords.dt0) {

	## doc term counts
        spk.tcount <- xwords.dt0[clean.word != "",list(tcount=length(niteid)),by=list(conv,spk, clean.word)]

	## doc length in words
        doc.word.count <- xwords.dt0[clean.word != "",list(wcount=length(clean.word)),by=list(conv,spk)]

	## doc term frequency
        setkey(spk.tcount, conv, spk)
        setkey(doc.word.count, conv, spk)
        spk.tf <- doc.word.count[spk.tcount]
        spk.tf <- spk.tf[,list(tf=tcount/wcount, tcount=tcount, wcount=wcount), by=list(conv,spk, clean.word)]

	return(spk.tf)
}

get.tf.conv <- function(xwords.dt0) {
	## doc term counts
        conv.tcount <- xwords.dt0[clean.word != "",list(tcount=length(niteid)),by=list(conv, clean.word)]

	## doc length in words
        doc.word.count <- xwords.dt0[clean.word != "",list(wcount=length(clean.word)),by=list(conv)]

	## doc term frequency
        setkey(conv.tcount, conv)
        setkey(doc.word.count, conv)
        conv.tf <- doc.word.count[conv.tcount]
        conv.tf <- conv.tf[,list(tf=tcount/wcount, tcount=tcount, wcount=wcount), by=list(conv, clean.word)]

	return(conv.tf)
}


get.tfidf.spk <- function(xwords.dt0, word.idf=NULL) {

	## get term frequency
	print("get.tf.spk")
	spk.tf <- get.tf.spk(xwords.dt0)

	if (is.null(word.idf)) {
		word.idf <- get.idf.spk(xwords.dt0)
	}

        setkey(word.idf, clean.word)
        setkey(spk.tf, clean.word)

	#print(spk.tf[,length(tf),by=clean.word][order(V1)])
	#print(word.idf[,length(D),by=clean.word][order(V1)])

	new.idf <- max(word.idf$idf, na.rm=T)

	
        spk.tf.df <- word.idf[spk.tf]
	print("spk.tf.df")
	spk.tf.df$idf[is.na(spk.tf.df$idf)] <- new.idf
        spk.tf.idf <- spk.tf.df[,list(tcount=tcount, tf=tf, idf=idf, tf.idf=tf * idf), by=list(conv, spk, clean.word)]

        return(spk.tf.idf)
}

get.tfidf.conv <- function(xwords.dt0, word.idf=NULL) {
	## get term frequency
	conv.tf <- get.tf.conv(xwords.dt0)

	if (is.null(word.idf)) {
		word.idf <- get.idf.conv(xwords.dt0)
	}

        setkey(word.idf, clean.word)
        setkey(conv.tf, clean.word)

	new.idf <- max(word.idf$idf, na.rm=T)

        conv.tf.df <- word.idf[conv.tf]
	conv.tf.df$idf[is.na(conv.tf.df$idf)] <- new.idf
        conv.tf.idf <- conv.tf.df[,list(tcount=tcount, tf=tf, idf=idf, tf.idf=tf * idf), by=list(conv, clean.word)]

        return(conv.tf.idf)
}

get.tfidf.grp <- get.tfidf.conv 

######################################################################################
##---------------------------------------------------------------------------
## Randoms 
get.lda.models <- function(dtm, k=100) {
	SEED <- 2010
	dtm.ldas <- list(VEM=LDA(dtm, k = k, control = list(seed = SEED)), 	
		VEM_fixed = LDA(dtm, k = k, 
			control = list(estimate.alpha = FALSE, seed = SEED)),
		Gibbs = LDA(dtm, k = k, method = "Gibbs",
			control = list(seed = SEED, burnin = 1000,
			thin = 100, iter = 1000)),
		CTM = CTM(dtm, k = k,
			control = list(seed = SEED,
			var = list(tol = 10^-4), em = list(tol = 10^-3))))

	return(dtm.ldas)
			
}
get.query.window <- function(tdocs, tcorpus, simsets, xconv, xstart, xend) {
	u <- tdocs[conv==xconv][wstart < xstart][wend > xend]	
	currsims <- simsets[conv==xconv][xstart < endtime][xend > starttime]

	return(list(docs=u, ix=which(names(tcorpus) %in% u[,wid]), sims=currsims))		
}

read.utep.tag.files <- function(recdir) {
	filenames <- list.files(recdir, pattern="*.tag.flat")
	recx <- data.table()
	for (filename in filenames) {
		seg.filename <- paste(recdir, filename, sep="/")
		trans.filename <- gsub("flat","annot",seg.filename) 
		print(c(seg.filename, trans.filename))

		segs <- data.table(read.delim(seg.filename, header=FALSE))
		setnames(segs, c("conv", "signal", "seg.id", "offset", "X") )
		segs$X <- NULL
		setkey(segs, seg.id, conv)	

		trans <- data.table(read.delim(trans.filename, header=FALSE))
		setnames(trans, c("conv", "trans.id", "start", "end", "text", "X") )
		trans$X <- NULL
		setkey(trans, start, conv)

		u <- segs[trans]
		setnames(u, c("seg.id", "offset"), c("start.id", "starttime")) 
		setkey(u, end, conv)
	
		v <- segs[u]
		setnames(v, c("seg.id", "offset"), c("end.id", "endtime")) 
		v$conv <- substr(v$conv,1, 8)

		recx <- rbind(recx, v[,list(conv,trans.id, signal,starttime,endtime,text)][order(starttime)])
		
	}

	return(recx)
} 


read.mlf.files <- function(recdir) {
	filenames <- list.files(recdir, pattern="*.mlf.melt")
	recx <- data.table()
	for (filename in filenames) {
		currfile <- paste(recdir, filename, sep="/")
		print(currfile)
		x <- data.table(read.table(currfile))
		recx <- rbind(recx, x)
		
	}
	setnames(recx, c("segid","conv","spk","seg.start","seg.end","starttime","endtime","trigram","word"))
	recx$seg.start <- recx$seg.start/100 
	recx$seg.end  <- recx$seg.end/100 
	recx$starttime <- recx$starttime * 10^(-7) + recx$seg.start 
	recx$endtime <- recx$endtime * 10^(-7) + recx$seg.start 

	return(recx)
} 

##---------------------------------------------------------------------------
## Aggregates
##---------------------------------------------------------------------------

#get.tf.aggs <- function(xdt, windows=NULL, wkey="xid", wsize=60, tstep=30) {
#        print(unique(xdt$conv))
#        f0.aggs.spk <- ddply(xdt, c("conv"), get.xint.windows,
#                        windows=windows, wkey=wkey, wsize=wsize, tstep=tstep, fx=get.tf.aggs.window)
#        return(data.table(f0.aggs.spk))
#}
#
#get.tf.aggs.window <- function(x,xprops) {
#        currstart <- unique(x$wstarts)
#        currend <- unique(x$wends)
#	#print(c(currstart,currend))
#        if (length(currend) >1) {
#                print(x)
#                print("Non unique end")
#        }
#        currconv <- unique(xprops$conv)
#        currx <- xprops[starttime < currend & endtime > currstart]
#	if (nrow(currx) > 0) {
#        	curr.aggs <- currx[,length(conv),by=list(word)]
#        	xstats <- data.table(wstart=currstart, wend=currend, curr.aggs)
#	} else {
#		xstats <- NULL
#	}
#
#
#        if ("niteid" %in% names(x)) {
#                xstats <- data.table(niteid=unique(x$niteid), xstats)
#        }
#        return(xstats)
#
#}
#

#get.wdocs <- function(xdt, windows=NULL, wkey="xid", wsize=30, tstep=10) {
#        wdocs <- ddply(xdt, c("conv"), get.xint.windows,
#                        windows=windows, wkey=wkey, wsize=wsize, tstep=tstep, fx=get.wdocs.window, .parallel=T)
#        return(data.table(wdocs))
#}
#
#
#get.wdocs.window <- function(x,xprops) {
#        currstart <- unique(x$wstarts)
#        currend <- unique(x$wends)
#	#print(c(currstart,currend))
#        if (length(currend) >1) {
#                print(x)
#                print("Non unique end")
#        }
#        currconv <- unique(xprops$conv)
#        currx <- xprops[starttime < currend & endtime > currstart]
#
#	if (nrow(currx) > 0) {
#        	curr.aggs <- data.table(doc.len=nrow(currx), doc.unique=currx[,length(unique(clean.word))], 
#					wrate=nrow(currx)/(currend-currstart),	
#					text=paste(currx[,word], collapse=" "))
#	} else {
#        	curr.aggs <- data.table(doc.len=0, doc.unique=0, wrate=0, text="")
#	}
#
#        xstats <- curr.aggs
#
#        if ("niteid" %in% names(x)) {
#                xstats <- data.table(niteid=unique(x$niteid), xstats)
#        }
#        return(xstats)
#
#}



get.niteid.spk <- function(x) {
	if (is.factor(x)) {	
		x <- unlevel(x)	
	} 
	#print(x)
	strsplit(x, split="\\.")[[1]][2]
}


## I think this was an attempt to make use of lapply to avoid some memory issues.
## Probably unnecessary.
get.tfidf.docs.conv <- function(xdt, windows, spk.only=T) {
        if (is.null(windows)) {
                print("No windows")
                return(NULL)
        }
        print("lapply")
        wdocs <- lapply(windows, get.tfidf.docs.window, xdt, spk.only=spk.only)
        print(warnings())
        print("unlist")
        wdocs <- unlist.df(wdocs)
        wdocs <- data.table(wid=wdocs[,paste(conv,wstart,wend,sep="-")], wdocs)
        print(wdocs[i])
        return(wdocs)
}


get.tfidf.docs.window <- function(x, xprops, spk.only=F) {
        #print("TFIDF")
        currstart <- unique(x$wstarts)
        currend <- unique(x$wends)
        currconv <- unique(xprops$conv)
        extra.words <- 0
        currspk <- get.niteid.spk(unique(x$niteid))


        if (length(currend) >1) {
                print(x)
                print("Non unique end")
        }

        currx <- xprops[starttime < currend & endtime > currstart]
        featnames <- names(xprops)

        if (nrow(currx) > 0) {
                extra.words <- nrow(currx[nxt_agent != currspk])
                if (spk.only) {
                        currx <- currx[nxt_agent == currspk]
                }
        }

        if (nrow(currx) > 0) {
                currwords <- currx$id
                sum.tf <- data.table(t(colSums(currx[, grep("^t[fp]", featnames), with=F])))
                sum.su <- data.table(t(colSums(currx[, grep("^su", featnames), with=F])))
                sum.pmi <- data.table(t(colSums(currx[, grep("^pmi", featnames), with=F])))

                max.tf <- data.table(t(apply(currx[, grep("^t[fp]", featnames), with=F],2,max)))
                max.su <- data.table(t(apply(currx[, grep("^su", featnames), with=F],2,max)))
                max.pmi <- data.table(t(apply(currx[, grep("^pmi", featnames), with=F],2,max)))
                #print(max.tf)
                #print(max.su)
                #print(max.pmi)


                setnames(max.tf, names(max.tf), gsub("^", "max.", names(max.tf)))
                setnames(max.su, names(max.su), gsub("^", "max.", names(max.su)))
                setnames(max.pmi, names(max.pmi), gsub("^", "max.", names(max.pmi)))

                curr.aggs <- data.table(doc.len=length(currwords), n.ovl.words=extra.words,
                        dur=currend-currstart, spk.rate=length(currwords)/(currend-currstart),
                        mean.word.F0=mean(currx$mean.normF0, na.rm=T),
                        mean.word.I0=mean(currx$mean.normI0, na.rm=T),
                        sum.tf, max.tf, sum.su, max.su, sum.pmi, max.pmi
                        )
                if (length(grep("^if", featnames)) > 0) {
                        sum.if <- data.table(t(colSums(currx[, grep("^if", featnames), with=F])))
                        max.if <- data.table(t(apply(currx[, grep("^if", featnames), with=F],2,max)))
                        setnames(max.if, names(max.if), gsub("^", "max.", names(max.if)))
                        curr.aggs <- data.table(curr.aggs, sum.if, max.if)
                }
                if (length(grep("^head", featnames)) > 0 ) {
                        sum.head <- data.table(t(colSums(currx[, grep("^head", featnames), with=F])))
                        max.head <- data.table(t(apply(currx[, grep("^head", featnames), with=F],2,max)))
                        setnames(max.head, names(max.head), gsub("^", "max.", names(max.head)))
                        curr.aggs <- data.table(curr.aggs, sum.head, max.head)
                }

        } else {
                #print("NO DATA")
                sum.tf <- data.table(t(colSums(xprops[1, grep("^t[fp]", featnames), with=F])*0))
                sum.su <- data.table(t(colSums(xprops[1, grep("^su", featnames), with=F])*0))
                sum.pmi <- data.table(t(colSums(xprops[1, grep("^pmi", featnames), with=F])*0))

                max.tf <- data.table(t(colSums(xprops[1, grep("^t[fp]", featnames), with=F])*0))
                max.su <- data.table(t(colSums(xprops[1, grep("^su", featnames), with=F])*0))
                max.pmi <- data.table(t(colSums(xprops[1, grep("^pmi", featnames), with=F])*0))

                setnames(max.tf, names(max.tf), gsub("^", "max.", names(max.tf)))
                setnames(max.su, names(max.su), gsub("^", "max.", names(max.su)))
                setnames(max.pmi, names(max.pmi), gsub("^", "max.", names(max.pmi)))


                curr.aggs <- data.table(doc.len=0, n.ovl.words=0,
                        dur=currend-currstart, spk.rate=0,
                        mean.word.F0=0, mean.word.I0=0,
                        sum.tf, max.tf, sum.su, max.su, sum.pmi, max.pmi
                        )
                if (length(grep("^if", featnames)) > 0) {
                        sum.if <- data.table(t(colSums(xprops[1, grep("^if", featnames), with=F])*0))
                        max.if <- data.table(t(colSums(xprops[1, grep("^if", featnames), with=F])*0))
                        setnames(max.if, names(max.if), gsub("^", "max.", names(max.if)))
                        curr.aggs <- data.table(curr.aggs, sum.if, max.if)
                }
                if (length(grep("^head", featnames)) > 0 ) {
                        sum.head <- data.table(t(colSums(xprops[1, grep("^head", featnames), with=F])*0))
                        max.head <- data.table(t(colSums(xprops[1, grep("^head", featnames), with=F])*0))
                        setnames(max.head, names(max.head), gsub("^", "max.", names(max.head)))
                        curr.aggs <- data.table(curr.aggs, sum.head, max.head)
                }
      	}

        xstats <- data.table(conv=currconv, wstart=currstart, wend=currend, curr.aggs)
        #print(xstats)

        if ("niteid" %in% names(x)) {
                xstats <- data.table(niteid=unique(x$niteid), xstats)
        }
        #print(xstats[1])
        #print("HERE")
        return(xstats)

}


#get.tf.docs.window <- function(x,xprops,spk.only=F) {
#        currstart <- unique(x$wstarts)
#        currend <- unique(x$wends)
#        currconv <- unique(xprops$conv)
#
#        if (length(currend) >1) {
#                print(x)
#                print("Non unique end")
#        }
#
#        currx <- xprops[starttime < currend & endtime > currstart]
#	featnames <- names(xprops) 
#
#	if (nrow(currx) > 0) {
#		currwords <- currx$id
#		sum.tf <- data.table(t(colSums(currx[, grep("^tf", featnames), with=F])))	
#		sum.su <- data.table(t(colSums(currx[, grep("^su", featnames), with=F])))	
#
#        	curr.aggs <- data.table(doc.len=length(currwords),
#			sum.tf, sum.su
#			) 
#	} else {
#
#		sum.tf <- data.table(t(colSums(xprops[1, grep("^tf", featnames), with=F])*0))	
#		sum.su <- data.table(t(colSums(xprops[1, grep("^su", featnames), with=F])*0))	
#
#        	curr.aggs <- data.table(doc.len=0, 
#			sum.tf, sum.su
#			)
#	}
#
#        xstats <- data.table(wstart=currstart, wend=currend, curr.aggs)
#
#        if ("niteid" %in% names(x)) {
#                xstats <- data.table(niteid=unique(x$niteid), xstats)
#        }
#        return(xstats)
#
#}
#
#
#
#
#get.tf.docs <- function(xdt, windows=NULL, wkey="xid", wsize=15, tstep=5, xvars=c("conv")) {
#        wdocs <- data.table(ddply(xdt, xvars, get.xint.windows,
#                        windows=windows, wkey=wkey, wsize=wsize, tstep=tstep, fx=get.tf.docs.window))
#	wdocs <- data.table(wid=wdocs[,paste(conv,wstart,wend,sep="-")], wdocs)	
#        return(wdocs)
#}
#

#get.tfidf.docs <- function(xdt, windows=NULL, wkey="xid", wsize=15, tstep=5, xvars=c("conv"), spk.only=F) {
#        wdocs <- data.table(ddply(xdt, xvars, get.xint.windows,
#                        windows=windows, wkey=wkey, wsize=wsize, tstep=tstep, fx=get.tfidf.docs.window, spk.only=spk.only))
#	wdocs <- data.table(wid=wdocs[,paste(conv,wstart,wend,sep="-")], wdocs)	
#        return(wdocs)
#}

#get.wcount.conv <- function(xdt, windows, spk.only=F) {
#	wdocs <- NULL
#	
#	for (i in 1:nrow(windows)) {
#		curr.wcount <- get.wcount.docs.window(windows[i], xdt, spk.only=spk.only)   
#		wdocs <- rbind(wdocs, curr.wcount)  
#	}
#        return(wdocs)
#}
#
#get.wcount.docs <- function(xdt, windows=NULL, wkey="xid", wsize=15, tstep=5, xvars=c("conv"), spk.only=F) {
#        wdocs <- data.table(ddply(xdt, xvars, get.xint.windows,
#                        windows=windows, wkey=wkey, wsize=wsize, tstep=tstep, fx=get.wcount.docs.window, spk.only=spk.only))
#	wdocs <- data.table(wid=wdocs[,paste(conv,wstart,wend,sep="-")], wdocs)	
#        return(wdocs)
#}
#
#
#get.wcount.docs.window <- function(x, xprops, spk.only=F) {
#        currstart <- unique(x$wstarts)
#        currend <- unique(x$wends)
#        currconv <- unique(xprops$conv)
#	extra.words <- 0  
#	currid <- unlevel(x$niteid[1])
#	print(currid)
#	currspk <- get.niteid.spk(currid)
#
#        if (length(currend) >1) {
#                print(x)
#                print("Non unique end")
#        }
#
#        currx <- xprops[starttime < currend & endtime > currstart]
#	featnames <- names(xprops) 
#
#	if (nrow(currx) > 0) {
#		wordspks <- currx[, list(spk=get.niteid.spk(id)), by=list(id)]$spk
#		extra.words <- nrow(currx[wordspks != currspk])
#		if (spk.only) {
#			currx <- currx[wordspks == currspk]
#		} 
#
#		currwords <- currx$id
#        	curr.aggs <- data.table(doc.len.raw=length(currwords), n.ovl.words.raw=extra.words,
#			dur.raw=currend-currstart, w.rate=length(currwords)/(currend-currstart)
#			) 
#
#	} else {
#		curr.aggs <- data.table(doc.len.raw=0, n.ovl.words.raw=0, dur.raw=0, w.rate=0)
#	}
#
#        xstats <- data.table(wstart=currstart, wend=currend, curr.aggs)
#	#print(xstats)
#
#        if ("niteid" %in% names(x)) {
#                xstats <- data.table(niteid=unique(x$niteid), xstats)
#        }
#	#print("HERE")
#        return(xstats)
#
#}
#

#get.nxtword.docs.window <- function(x,xprops) {
#        currstart <- unique(x$wstarts)
#        currend <- unique(x$wends)
#        currconv <- unique(xprops$conv)
#
#        if (length(currend) >1) {
#                print(x)
#                print("Non unique end")
#        }
#        currx <- xprops[starttime < currend & endtime > currstart]
#
#
#	if (nrow(currx) > 0) {
#		currwords <- currx$word
#        	curr.aggs <- data.table(doc.len=length(currwords), doc.unique=length(unique(currwords)), 
#			sum.tf.idf=sum(currx$tf.idf), sum.idf=sum(currx$idf), 	
#			text=paste(currwords, collapse=" "))
#	} else {
#        	curr.aggs <- data.table(doc.len=0, doc.unique=0, sum.tf.idf=0, sum.idf=0, text="")
#	}
#
#        xstats <- data.table(wstart=currstart, wend=currend, curr.aggs)
#	#print(xstats)
#
#        if ("niteid" %in% names(x)) {
#                xstats <- data.table(niteid=unique(x$niteid), xstats)
#        }
#	#print("HERE")
#        return(xstats)
#
#}
#
#
#get.nxtword.docs <- function(xdt, windows=NULL, wkey="xid", wsize=15, tstep=5, xvars=c("conv")) {
#        wdocs <- ddply(xdt, xvars, get.xint.windows,
#                        windows=windows, wkey=wkey, wsize=wsize, tstep=tstep, fx=get.nxtword.docs.window)
#        return(data.table(wdocs))
#}


############################################################################
# Some functions for the utep Mediaeval data
############################################################################

get.tdocs.window <- function(x,xprops) {
        currstart <- unique(x$wstarts)
        currend <- unique(x$wends)
        currconv <- unique(xprops$conv)
	#print(c(currconv, currstart,currend))

        if (length(currend) >1) {
                print(x)
                print("Non unique end")
        }
        currx <- xprops[starttime < currend & endtime > currstart]
	#print(currx)	


	if (nrow(currx) > 0) {
		currwords <- unlist(strsplit(currx[, unlevel(text)], " "))	
		#print(currwords)
        	curr.aggs <- data.table(doc.len=length(currwords), doc.unique=length(unique(currwords)), 
			text=paste(currx[,text], collapse=" "))
	} else {
        	curr.aggs <- data.table(doc.len=0, doc.unique=0, text="")
	}

        xstats <- data.table(wstart=currstart, wend=currend, curr.aggs)
	#print(xstats)

        if ("niteid" %in% names(x)) {
                xstats <- data.table(niteid=unique(x$niteid), xstats)
        }
	#print("HERE")
        return(xstats)

}


get.tdocs <- function(xdt, windows=NULL, wkey="xid", wsize=60, tstep=20) {
        wdocs <- ddply(xdt, c("conv"), get.xint.windows,
                        windows=windows, wkey=wkey, wsize=wsize, tstep=tstep, fx=get.tdocs.window)
        return(data.table(wdocs))
}



get.simsets.window <- function(x,xprops) {
        currstart <- unique(x$wstarts)
        currend <- unique(x$wends)
        currconv <- unique(xprops$conv)
	#print(c(currconv, currstart,currend))

        if (length(currend) >1) {
                print(x)
                print("Non unique end")
        }
        currx <- xprops[starttime < currend & endtime > currstart]
	#print(currx)	


	if (nrow(currx) > 0) {
		#print(currwords)
        	curr.aggs <- data.table(simset.len=nrow(currx),
			tag=currx[,unlevel(tag)])
	} else {
        	curr.aggs <- data.table(simset.len=0, tag="")
	}

        xstats <- data.table(wstart=currstart, wend=currend, curr.aggs)

        if ("niteid" %in% names(x)) {
                xstats <- data.table(niteid=unique(x$niteid), xstats)
        }
        return(xstats)
}


get.simsets.all <- function(xdt, windows=NULL, wkey="xid", wsize=60, tstep=20) {
        wdocs <- data.table(ddply(xdt, c("conv"), get.xint.windows,
                        windows=windows, wkey=wkey, wsize=wsize, tstep=tstep, fx=get.simsets.window))
	wdocs <- data.table(wid=wdocs[,paste(conv,wstart,wend,sep="-")], wdocs)
        return(wdocs)
}



main.utep <- function() {

	## Edin-ASR
	transfiles <- read.mlf.files("./utep")
	transfiles <- transfiles[!grep("^<", word)]

	## Utep manual transcripts
	trans.utep <- read.utep.tag.files("./utep/all-transcripts")
	trans.utep <- trans.utep[!is.na(endtime)]
	tdocs <- get.tdocs(trans.utep)


	tcorpus <- Corpus(VectorSource(tdocs[doc.len > 0,text]))
	names(tcorpus) <- tdocs[doc.len > 0,paste(conv,wstart,wend, sep="-")]
	tcorpus <- tm_map(tcorpus, function(x) {gsub("[?]", " ? ", x)})
	tcorpus <- tm_map(tcorpus, function(x) {gsub("[!]", " ! ", x)})
	tcorpus <- tm_map(tcorpus, stripWhitespace)
	tcorpus <- tm_map(tcorpus, tolower)
	tcorpus <- tm_map(tcorpus, function(x) {gsub("[=\\.,/`-]", "", x)})
	tcorpus <- tm_map(tcorpus, stemDocument, language="en")
	tdm <- TermDocumentMatrix(tcorpus)	
	tdm2 <- TermDocumentMatrix(tcorpus, control=list(weighting=function(x){weightTfIdf(x, normalize=F)}))	

	dtm <- DocumentTermMatrix(tcorpus, 	
		control = list(stopwords = TRUE, minWordLength = 3,
		removeNumbers = TRUE))

#
	sapply(dtm.ldas, function(x) {
		mean(apply(posterior(x)$topics, 1 , function(z) - sum(z * log(z))))
			 } )

	
	simsets <- data.table(read.delim("utep/all-simsets-clean.txt.melt", header=F))
	setnames(simsets, c("V1","tag","eaf","starttime","endtime"))	

}

