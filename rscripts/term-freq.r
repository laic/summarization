## term-freq.r: functions for calculating term-fequency features
## was called ir.r 
## used by: proc-lex.r (join?) 

source("../rscripts/nxt-proc.r")
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

get.tfidf.grp <- 
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


######################################################################################
## Calculate aggregates
######################################################################################

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

## This works but its all a bit clunky! Redo 
get.tfidf.docs.window <- function(x, xprops, spk.only=F) {
        print("TFIDF")
        currstart <- unique(x$wstarts)
        currend <- unique(x$wends)
        currconv <- unique(xprops$conv)
        extra.words <- 0
        currspk <- get.niteid.spk(unique(x$niteid))

	print(c(currstart, currend, currconv, currspk)) 

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

        if ("niteid" %in% names(x)) {
                xstats <- data.table(niteid=unique(x$niteid), xstats)
        }
        return(xstats)
}





