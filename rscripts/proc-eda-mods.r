library(data.table)
library(plyr)
source("../rscripts/f0basics.r")
source("../rscripts/extsumm-eval.r")
source("../rscripts/proc-lme.r")
library(xtable)
library(arm)

#-------------------------------------------------------------------
# Some feature set names
#-------------------------------------------------------------------
nonpredict.feats <- c("conv", "annot", "mtype", "mgroup", "V3", "wid", 
	"niteid", "xid", "spk.id", "pid", "participant", "spk", "nspurts", "starttime",
	"endtime", "datype", "wstart", "wend", "eda.true", "eda.time", "link.eda")

murray.prosody <- c("mean.normF0","q97.5.normF0","sd.normF0",  
		"mean.normI0","q97.5.normI0",
		"time.from.prevs", "time.to.nexts", "spk.rate")

murray.prosody2 <- c("mean.normF0","q97.5.normF0","sd.normF0",  
		"mean.normI0","q97.5.normI0",
		"time.from.prevs", "time.to.nexts", "w.rate")

murray.prosody.esps <- c("mean.normF0.esps","q97.5.normF0.esps","sd.normF0.esps",  
		"mean.normI0.esps","q97.5.normI0.esps",
		"time.from.prevs", "time.to.nexts", "w.rate")

murray.len <- c("dur.1", "uninterrupted", "doc.len")

murray.len2 <- c("dur.1", "uninterrupted", "doc.len.raw")
murray.spk <- c("spk.dom.prop", "spk.dom.da")
murray.struct <- c("trank","mpos")
murray.lex <- c("tf.idf","su.idf")

murray.feats <- list(list(fname="murray.prosody", featnames=murray.prosody), 
		list(fname="murray.len", featnames=murray.len), 
		list(fname="murray.spk", featnames=murray.spk), 
		list(fname="murray.struct", featnames=murray.struct), 
		list(fname="murray.lex", featnames=murray.lex), 
		list(fname="murray.all", featnames=c(murray.prosody, murray.len, murray.spk, murray.struct, murray.lex)))


murray.lex2 <- c("su.dur.if.0","tf.idf")
murray.all2 <- c(murray.prosody, murray.len, murray.spk, murray.struct, murray.lex2)
murray.all <- c(murray.prosody, murray.len, murray.spk, murray.struct, murray.lex)

murray.feats2 <- list(murray.prosody2, murray.len2, murray.spk, murray.struct, murray.lex)

#-------------------------------------------------------------------
# Write out feature subsets with or without z-score normalization
write.feat.dts <- function(group.fx0, nonpredict.feats, zscore=T, test.part=NULL) {
	for (xfeat in setdiff(names(group.fx0), nonpredict.feats)) {
		curr.feats <- c(xfeat)
		print(curr.feats)
		if (is.factor(group.fx0[[xfeat]])) {
			print(c("FACTOR", xfeat))
		} else {
			if (zscore) {
				group.fx <- get.zscore.obs(group.fx0, featnames=curr.feats) 
				outdir <- "/disk/scratch/ami/derived/segs/zfeat/" 
			} else {
				group.fx <- get.raw.obs(group.fx0, featnames=curr.feats) 
				outdir <- "/disk/scratch/ami/derived/segs/rawfeat/" 
			}
			if (is.null(test.part)) {
				outfile.txt <- paste(outdir, xfeat, ".txt", sep="") 
				write.table(group.fx, file=outfile.txt, row.names=F)
			} else {
				eval.fx <- group.fx[mgroup %in% test.part[["test"]]]	
				evalfile.txt <- paste(outdir, xfeat, "-eval.txt", sep="") 
				write.table(eval.fx, file=evalfile.txt, row.names=F)

				if (!is.null(test.part[["dev"]])) {
					dev.fx <- group.fx[mgroup %in% test.part[["dev"]]]	
					devfile.txt <- paste(outdir, xfeat, "-dev.txt", sep="") 
					write.table(dev.fx, file=devfile.txt, row.names=F)
				}

				train.fx <- group.fx[!(mgroup %in% c(test.part[["dev"]],test.part[["test"]]))]	
				trainfile.txt <- paste(outdir, xfeat, "-train.txt", sep="") 
				write.table(train.fx, file=trainfile.txt, row.names=F)
			}
		}
	}

	return()

}

write.murray.feats <- function(murray.feats, group.fx0, nonpredict.feats, zscore=F, test.part=NULL) {
	lapply(murray.feats, write.feat.set.dts, group.fx0=group.fx0, 
			nonpredict.feats=nonpredict.feats, zscore=zscore, test.part=test.part)
}

write.feat.set.dts <- function(x.pred.feats, group.fx0, nonpredict.feats, zscore=F, test.part=NULL) {
	if (is.list(x.pred.feats)) {
		pred.feats <- x.pred.feats[["featnames"]]	
		xfeat <- x.pred.feats[["fname"]]	
	} else {
		pred.feats <- x.pred.feats
		xfeat <- paste(pred.feats, collapse="-") 
	}
	print(x.pred.feats)
	print(pred.feats)

	if (zscore) {
		print("zscore")	
		group.fx <- get.zscore.obs(group.fx0, featnames=pred.feats) 
		outdir <-"/disk/scratch/ami/derived/segs/zfeat/"
	} else {
		group.fx <- get.raw.obs(group.fx0, featnames=pred.feats) 
		outdir <-"/disk/scratch/ami/derived/segs/rawfeat/"
	}


	if (is.null(test.part)) {
		outfile.txt <- paste(outdir, xfeat, ".txt", sep="") 
		print(outfile.txt)
		write.table(group.fx, file=outfile.txt, row.names=F)
	} else {
		eval.fx <- group.fx[mgroup %in% test.part[["test"]]]	
		evalfile.txt <- paste(outdir, xfeat, "-eval.txt", sep="") 
		print(c(evalfile.txt, nrow(eval.fx)))
		write.table(eval.fx, file=evalfile.txt, row.names=F)

		if (!is.null(test.part[["dev"]])) {
			dev.fx <- group.fx[mgroup %in% test.part[["dev"]]]	
			devfile.txt <- paste(outdir, xfeat, "-dev.txt", sep="") 
			print(c(devfile.txt, nrow(dev.fx)))
			write.table(dev.fx, file=devfile.txt, row.names=F)
		}

		train.fx <- group.fx[!(mgroup %in% c(test.part[["dev"]],test.part[["test"]]))]	
		trainfile.txt <- paste(outdir, xfeat, "-train.txt", sep="") 
		print(c(trainfile.txt, nrow(train.fx)))
		write.table(train.fx, file=trainfile.txt, row.names=F)

	}

	return(0)

}

write.feats.plus.dts <- function(group.fx0, nonpredict.feats, always.in.feats=NULL) {
	for (xfeat in setdiff(names(group.fx0), c(always.in.feats, nonpredict.feats))) {
		curr.feats <- c(always.in.feats, xfeat)
		print(curr.feats)
		if (is.factor(group.fx0[[xfeat]])) {
			print(c("FACTOR", xfeat))
		} else {
			group.fx <- get.zscore.obs(group.fx0, featnames=curr.feats) 
			outfile.txt <- paste("/disk/scratch/ami/derived/segs/mod/", 
						paste(always.in.feats, collapse="-"), "-",
						xfeat, ".txt", sep="") 
			write.table(group.fx, file=outfile.txt, row.names=F)

		}
	}

	return()
}


get.feat.preds <- function(group.fx0, nonpredict.feats) {
	xfeat.preds <- list()	 
	for (xfeat in setdiff(names(group.fx0), nonpredict.feats)[1:3]) {
		curr.feats <- c(xfeat)
		print(curr.feats)
		group.fx <- get.zscore.obs(group.fx0, featnames=curr.feats) 
		m <- mod.edag.xfeats(group.fx, featnames=curr.feats); display(m$mod)
		cv <- get.cv.wrapper(m, xdata=group.fx, x.formula=m$x.form, nfolds=10, N="", spk=F)
		x.pred <- list(m=m, cv=cv)
		outfile <- paste("/disk/scratch/ami/derived/segs/mod/", xfeat, ".obj", sep="") 
		save(x.pred, file=outfile)
		xfeat.preds[[xfeat]] <- x.pred
	}

	return(xfeat.preds)

}

#add.linked.eda <- function(abs.ext, group.fx0) {
#	if (!is.data.table(abs.ext)) {
#		abs.ext <- data.table(abs.ext)
#	}
#	u <- abs.ext[, list(n.links=length(slink.id)),by=list(annot, niteid)]
#
#	setkey(u, annot, niteid)
#	setkey(group.fx0, annot, niteid)
#	link.da <- u[group.fx0][!is.na(n.links)][, niteid]
#	xeda <- data.table(group.fx0, link.eda=group.fx0[, niteid %in% link.da])
#
#	return(xeda)	
#
#}

get.raw.obs <- function(group.fx0, featnames=c("tf.idf")) {
        u <- group.fx0[, list(wid=niteid, niteid, annot, mtype, mgroup,       
                eda.time=eda.time, eda.true=eda.true, link.eda=link.eda)]
        setkey(u, niteid)
        for (featname in featnames) {
		print(featname)
                curr <- group.fx0[, list(niteid, xval=get(featname)),]
		curr$xval[is.na(curr$xval)] <- min(curr$xval, na.rm=T)	
                setnames(curr, c("xval"), c(featname))
                setkey(curr, niteid)
                setkey(u, niteid)
                u <- u[curr]
        }

        return(u)
}

get.zscore.obs <- function(group.fx0, featnames) {
	print("Zscore")	
	print(length(names(group.fx0)))
        u <- group.fx0[, list(wid=niteid, conv, starttime, endtime, niteid, annot, mtype, mgroup,       
                eda.time=eda.time, eda.true=eda.true, link.eda=link.eda)]
        setkey(u, niteid, annot)

	if (sum(!(featnames %in% names(group.fx0))) > 0) {
		return(NULL)
	}

        for (featname in featnames) {
		print(featname)
                curr <- group.fx0[, list(niteid, annot, xval=to.zscore(get(featname))),]
		print(min(curr$xval, na.rm=T))
		#set NAs to min values
		curr$xval[is.na(curr$xval)] <- min(curr$xval, na.rm=T)	
                setnames(curr, c("xval"), c(featname))
                setkey(curr, niteid, annot)
                setkey(u, niteid, annot)
                u <- u[curr]
        }

        return(u)
}

#------------------------------------------------------------------------------
# Some LR wrappers
#------------------------------------------------------------------------------

mod.glmlr.xfeats <- function(u, featnames, dep.var="link.eda" ) {
        form.dep <- paste(dep.var, " ~  ", sep="")
        #print(form.dep)
        x.form <- as.formula(paste(c(form.dep, featnames), collapse=" + ") )
        #print(x.form)
        m0 <- glm(x.form ,data = u, family = binomial(link = "logit"))
        return(list(mod=m0, x.form=x.form))
}


## This is the generic one we'll use:
mod.edag.xfeats <- function(u, featnames, dep.var="eda.true" ) {
        form.dep <- paste(dep.var, " ~  (1 | mtype) + (1 | annot) ", sep="")
        #print(form.dep)
        x.form <- as.formula(paste(c(form.dep, featnames), collapse=" + ") )
        #print(x.form)
        m0 <- lmer(x.form ,data = u, family = binomial(link = "logit"))
        return(list(mod=m0, x.form=x.form))
}

#------------------------------------------------------------------------------
# Some post-hoc stats
#------------------------------------------------------------------------------

## Cosine based redundancy
get.redundancy.icsi <- function() {
	da.word <- data.table(read.delim("~/data/icsi/derived/icsi.da.word.txt", header=F, fill=T))
	da.word$V4 <- NULL
	setnames(da.word, c("wid", "da.id", "word"))
	da.word$word <- tolower(gsub(" ", "", da.word$word))
	da.word$word <- gsub("[!.,?]", "", da.word$word)
	da.word$word <- gsub(" $", "", da.word$word)
	da.word$wid <- gsub(",", "", da.word$wid)

	load("~/data/misc-derived/all.lex.grp")

	evaldir <- "~/data/icsi/derived/segs/reval"
	all.sims <- cosine.sim.all(evaldir=evaldir, da.word=da.word, all.lex.grp=all.lex.grp)
	save(all.sims, file="~/data/icsi/derived/icsi.all.sims")
}
get.redundancy.ami <- function(pattern="eval.txt.sums$") {
	da.word <- data.table(read.delim("../ami.da.word.clean.txt", header=F, fill=T))
	setnames(da.word, c("wid", "da.id", "word"))
	da.word$word <- tolower(gsub(" ", "", da.word$word))
	da.word$word <- gsub("[!.,?]", "", da.word$word)
	da.word$word <- gsub(" $", "", da.word$word)
	da.word$wid <- gsub(",", "", da.word$wid)

	load("~/data/misc-derived/all.lex.grp")

	evaldir <- "~/data/ami/derived/segs/reval"
	all.sims <- cosine.sim.all(evaldir=evaldir, da.word=da.word, all.lex.grp=all.lex.grp, pattern=pattern)
	save(all.sims, file="~/data/ami/derived/ami.all.sims.head")
	return(all.sims)
}

cosine.sim.all <- function(evaldir, da.word, all.lex.grp, pattern="eval.txt.sums$") {
	fnames <- list.files(evaldir, pattern=pattern)

	fsim <- NULL
	for (fname in fnames) {
		print(fname)
		v  <- data.table(read.table(paste(evaldir,"/", fname, sep=""), header=T))
		currsim <- data.table(ddply(v, .(conv), da.cosine.sim, da.word=da.word, all.lex.grp=all.lex.grp))
		print("fsim")
		fsim <- rbind(fsim, data.table(fname=gsub("eval.txt.sums", "", fname), currsim[,list(sum.sim=sum(sim), mean.sim=mean(sim), n.da=length(sim)),by=conv]))
		print("write")
		write.table(currsim, file=paste(evaldir,"/", fname, ".sim", sep=""))
	}

	return(fsim)

}


da.cosine.sim <- function(v0, da.word, all.lex.grp) {
	print(unique(v0$conv))
	if (!is.data.table(v0)) {
		v0 <- data.table(v0)
	}
	y <- v0[is.sum==T]
	u <- da.word[da.id %in% y$niteid]
	setkey(u, wid)
	setkey(all.lex.grp, id)
	
	u.lex <- all.lex.grp[u][!is.na(conv)]

	u.tmat <- data.table(dcast(u.lex, formula=da.id ~ clean.word, value.var="tf.idf.grp",fun.aggregate=max, fill=0))	

	da.sim <- NULL

	for (i in 1:nrow(u.tmat)) {
		#print(i)
		currid <- u.tmat$da.id[i]
		curr.v <- as.vector(u.tmat[i,grep("da.id", names(u.tmat),invert=T),with=F])
		#print(curr.v)		

		x.tmat <- u.tmat[da.id != currid]
		rest.v0 <- x.tmat[,grep("da.id", names(x.tmat),invert=T),with=F]
		rest.v <-  as.vector(apply(rest.v0,2,max))

		norm.curr <- sqrt(sum(curr.v * curr.v)) 
		norm.rest <- sqrt(sum(rest.v * rest.v)) 

		sim <- (sum(curr.v * rest.v))/(norm.curr * norm.rest) 

		da.sim <- rbind(da.sim, data.table(da.id=currid, sim=sim))
	}


	return(da.sim)

}

conv.cosine.sim <- function(v0, da.word, all.lex.grp) {
	print(unique(v0$conv))
	if (!is.data.table(v0)) {
		v0 <- data.table(v0)
	}
	y <- v0[is.sum==T]
	u <- da.word[da.id %in% y$niteid]
	setkey(u, wid)
	setkey(all.lex.grp, id)
	
	u.lex <- all.lex.grp[u][!is.na(conv)]

	u.tmat <- data.table(dcast(u.lex, formula=da.id ~ clean.word, value.var="tf.idf.grp",fun.aggregate=max, fill=0))	

	da.sim <- NULL

	for (i in 1:nrow(u.tmat)) {
		#print(i)
		currid <- u.tmat$da.id[i]
		curr.v <- as.vector(u.tmat[i,grep("da.id", names(u.tmat),invert=T),with=F])
		#print(curr.v)		

		x.tmat <- u.tmat[da.id != currid]
		rest.v0 <- x.tmat[,grep("da.id", names(x.tmat),invert=T),with=F]
		rest.v <-  as.vector(apply(rest.v0,2,max))

		norm.curr <- sqrt(sum(curr.v * curr.v)) 
		norm.rest <- sqrt(sum(rest.v * rest.v)) 

		sim <- (sum(curr.v * rest.v))/(norm.curr * norm.rest) 

		da.sim <- rbind(da.sim, data.table(da.id=currid, sim=sim))
	}


	return(da.sim)

}

############################################################################
## Proc output, write out summaries etc
############################################################################
proc.evals.fset  <- function(evaldir="~/data/icsi/derived/segs/reval", da.text, abs.ext=NULL, pattern="eval.txt$") {

	fnames <- list.files(evaldir, pattern=pattern)

	all.stats <- NULL
	for (fname in fnames) {
		print("¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬")
		print(fname)
		eval.sums <- get.eval.summaries(fname=fname, evaldir=evaldir, da.text) 
		if (!is.null(abs.ext)) {
			eval.sums$eda.true[eval.sums$niteid %in% abs.ext$niteid] <- T
		}
		eval.stats <- data.table(fname=fname, get.eval.summ.stats.all(eval.sums))	
		write.table(eval.stats, file=paste(evaldir,"/",fname, ".stats",sep=""))
		write.table(eval.sums, file=paste(evaldir,"/",fname, ".sums",sep=""))

		all.stats <- rbind(all.stats, eval.stats)
	}

	return(all.stats)	

}


get.eda.dist <- function(eval.sum) {
	print("eda.dist")
	print(unique(eval.sum$conv))
        wcover <- eval.sum[is.sum==T][order(starttime)]
	ecover <- eval.sum[eda.true==T]

	x <- Intervals(ecover[,list(starttime, endtime)])
	y <- Intervals(wcover[,list(starttime, endtime)])
	oyx <- interval_overlap(y,x)

	ds <- NULL
	for (i in 1:nrow(wcover)) {	
		#print(oyx[[i]])
		if (length(oyx[[i]])>0) {
			d <- 0
		} else {
			currstart <- wcover$starttime[i]		
			currid <- wcover$niteid[i]
			dstart <- abs(ecover$starttime[ecover$niteid != currid] - currstart)
			dend <- abs(ecover$endtime[ecover$niteid != currid] - currstart)
			#print(dstart)
			#print(dend)
			d <- min(dstart, dend)
		}
		ds <- c(ds, d)
		#print(d)
	}
	return(data.table(mean.dist=mean(ds, na.rm=T), max.dist=max(ds, na.rm=T), sd.dist=sd(ds,na.rm=T)))
}


get.eval.stats  <- function(evaldir="~/data/icsi/derived/segs/reval",  abs.ext=NULL) {

	fnames <- list.files(evaldir, pattern="eval.txt.sums$")
	all.stats <- NULL
	for (fname in fnames) {
		print("¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬")
		print(fname)
		eval.sums  <- data.table(read.table(paste(evaldir,"/", fname, sep=""), header=T))
		if (!is.null(abs.ext)) {
			eval.sums$eda.true[eval.sums$niteid %in% abs.ext$niteid] <- T
		}
		eval.stats <- data.table(fname=fname, get.eval.summ.stats.all(eval.sums))	
		write.table(eval.stats, file=paste(evaldir,"/",fname, ".stats.all",sep=""))
	#	write.table(eval.sums, file=paste(evaldir,"/",fname, ".sums",sep=""))
		all.stats <- rbind(all.stats, eval.stats)
	}

	return(all.stats)	

}

icsi.verbose.dist <- function() {
	eval.dir <- "~/data/icsi/derived/segs/reval/"	
	abs.names <- c("abs.id", "s.id", "slink.id", "niteid", "starttime","endtime","da.type")
        icsi.abs.ext <- get.abs.ext.dt(filename="~/data/icsi/derived/icsi.abs.da.txt", corpus="icsi", abs.names=abs.names)

	icsi.vdist <- all.verbose.dist(evaldir=eval.idr, abs.ext=icsi.abs.ext) 
}

ami.verbose.dist <- function() {
	eval.dir <- "~/data/ami/derived/segs/reval/"	
	abs.names <- c("abs.id", "s.id", "slink.id", "niteid", "starttime","endtime","da.type")
        ami.abs.ext <- get.abs.ext.dt(filename="~/data/ami/derived/ami.abs.ext.all.txt", corpus="ami", abs.names=abs.names)

	ami.vdist <- all.verbose.dist(evaldir=eval.idr, abs.ext=ami.abs.ext) 
}

all.verbose.dist <- function(evaldir, abs.ext=NULL) {

	fnames <- list.files(evaldir, pattern="eval.txt.sums$")

	fdist <- NULL
	for (fname in fnames) {
		print(fname)
		eval.sums  <- data.table(read.table(paste(evaldir,"/", fname, sep=""), header=T))
		if (!is.null(abs.ext)) {
			eval.sums$eda.true[eval.sums$niteid %in% abs.ext$niteid] <- T
		}
		currdist <- data.table(fname=fname, ddply(eval.sums, .(conv), get.eda.dist.verbose))
		fdist <- rbind(fdist, currdist)
		write.table(currdist, file=paste(evaldir,"/", fname, ".sum.dist", sep=""))

	}
	return(fdist)

}

get.eda.dist.verbose <- function(eval.sum) {
	print("eda.dist.verbose")
	if (!is.data.table(eval.sum)) {
		eval.sum <- data.table(eval.sum)
	}
	print(unique(eval.sum$conv))
        wcover <- eval.sum[is.sum==T][order(starttime)]
	ecover <- eval.sum[eda.true==T]

	x <- Intervals(ecover[,list(starttime, endtime)])
	y <- Intervals(wcover[,list(starttime, endtime)])
	oyx <- interval_overlap(y,x)

	ds <- NULL
	for (i in 1:nrow(wcover)) {	
		#print(oyx[[i]])
		currstart <- wcover$starttime[i]		
		currid <- wcover$niteid[i]
		if (length(oyx[[i]])>0) {
			d <- 0
		} else {
			dstart <- abs(ecover$starttime[ecover$niteid != currid] - currstart)
			dend <- abs(ecover$endtime[ecover$niteid != currid] - currstart)
			#print(dstart)
			#print(dend)
			d <- min(dstart, dend)
		}
		ds <- rbind(ds, data.table(wcover[i], dist=d))
		#print(d)
	}

	return(ds)
}

get.eval.summ.stats.all <- function(eval.sums) {
	return(data.table(ddply(eval.sums, c("conv"), get.eval.summ.stats)))	
}

get.eval.summ.stats <- function(eval.sum) {
	if (!is.data.table(eval.sum)) {
		eval.sum <- data.table(eval.sum)
	}

	#print(eval.sum)
	currconv <- unique(eval.sum$conv)
	print(currconv)	
	eda.dist <- get.eda.dist(eval.sum)

	maxtime <- max(eval.sum$starttime, na.rm=T)
	mean.dist <- eval.sum[is.sum==T][order(starttime), mean(diff(starttime))]
	wcover <- eval.sum[is.sum==T][order(starttime)]
	ecover <- eval.sum[eda.true==T]

	qs <- data.table(qstart=(maxtime/4) * c(0:3), qend=(maxtime/4) * c(1:4))
	qxs <- NULL
	for (i in 1:nrow(qs)) {
		npred <- wcover[starttime >=qs$qstart[i]][endtime <= qs$qend[i],length(niteid)] 
		durpred <- wcover[starttime >=qs$qstart[i]][endtime <= qs$qend[i],sum(endtime-starttime, na.rm=T)] 
		nannot <- ecover[starttime >=qs$qstart[i]][endtime <= qs$qend[i],length(niteid)] 
		durannot <- ecover[starttime >=qs$qstart[i]][endtime <= qs$qend[i],sum(endtime-starttime, na.rm=T)] 
		nda <- eval.sum[starttime >=qs$qstart[i]][endtime <= qs$qend[i],length(niteid)] 
		q.mean.dist <- wcover[starttime >=qs$qstart[i]][endtime <= qs$qend[i],mean(diff(starttime))] 
		
		select.rate <- npred/nda

		qx <- data.table(quartile=paste("q",i,sep=""), npred=npred, nannot=nannot, predprop=npred/nrow(wcover), 
			select.rate=select.rate, q.mean.dist=q.mean.dist, prop.annot=npred/nannot, dur.prop.annot=durpred/durannot, 
			dur.pred=durpred, dur.annot=durannot)
		qxs <- rbind(qxs, qx)

	}

	
	return(data.table(conv=currconv, mean.pred.dist=mean.dist, eda.dist, qxs))  

}



get.eval.summaries <- function(fname, evaldir="~/data/icsi/derived/segs/reval", da.text) {
	lvals <- data.table(read.table(paste(evaldir,fname,sep="/")))
	setkey(lvals, niteid, conv, starttime, endtime)

	if ("da.id" %in% names(da.text)) {
		setnames(da.text, c("da.id"), c("niteid"))
	}
	setkey(da.text, niteid, conv, starttime, endtime)
	lvals <- da.text[lvals]
	lvals$annot <- NULL

	setkey(lvals, niteid, conv)
	lvals <- unique(lvals)
	eval.sum <- data.table(ddply(lvals, .(conv), get.eval.summary.conv))

	return(eval.sum)
}

get.eval.summary.conv <- function(cv.text,  compression=0.15){
	currconv <- unique(cv.text$conv)
	print(currconv)
	if (!is.data.table(cv.text)) {
		cv.text <- data.table(cv.text)
	}
	if (length(is.na(cv.text$nwords)) > 0) {
		cv.text$nwords[is.na(cv.text$nwords)] <- 0
	}

	cv.text <- unique(cv.text)
	cv.text <- cv.text[order(logit.val,decreasing=T)]
	print(cv.text[is.na(nwords)])
	nwords <- sum(cv.text$nwords)  
	n10 <-round((nwords * compression) , 0)

	print(c(nwords, n10))
	nwords.sum <- 0	
	cv.select <- NULL 
	i <- 1
	while (nwords.sum < n10) {
		cv.select <- rbind(cv.select, cv.text[i])  	
		nwords.sum <- nwords.sum + cv.text[i]$nwords
		i <- i+1
	}
	
	cv.text <- data.table(cv.text, is.sum=cv.text[,niteid %in% cv.select$niteid])
	return(cv.text)
}

write.eval.html.all  <- function(evaldir="~/data/icsi/derived/segs/reval", da.text, outdir, pattern="eval.txt$") {

	fnames <- list.files(evaldir, pattern=pattern)
	print(fnames)
	for (fname in fnames) {
		print("¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬")
		print(fname)
		eval.sums <- get.eval.summaries.html(fname=fname, evaldir=evaldir, da.text=da.text, outdir=outdir) 

	}

	print("DONE")

}


get.eval.summaries.html <- function(fname, evaldir="~/data/icsi/derived/segs/reval", da.text, outdir) {
	print(fname)
	lvals <- data.table(read.table(paste(evaldir,fname,sep="/")))
	setkey(lvals, niteid, conv, starttime, endtime)

	if ("da.id" %in% names(da.text)) {
		setnames(da.text, c("da.id"), c("niteid"))
	}
	setkey(da.text, niteid, conv, starttime, endtime)
	lvals <- da.text[lvals]
	lvals$annot <- NULL

	setkey(lvals, niteid, conv)
	lvals <- unique(lvals)
	fset <- gsub("eval.txt$","",fname)
	print(fset)
	eval.sum <- data.table(ddply(lvals, .(conv), write.html.summary.conv, outdir=outdir, curr.feats=fset, compression=0.15))

	return(eval.sum)
}

write.html.summary.conv <- function(cv.text, outdir, curr.feats, compression=0.1, classifier="lmer"){
	print("###")
	currconv <- unique(cv.text$conv)
	print(currconv)
	if (!is.data.table(cv.text)) {
		cv.text <- data.table(cv.text)
	}
	print("here")
	#print(cv.text)
	if (length(is.na(cv.text$nwords)) > 0) {
		cv.text$nwords[is.na(cv.text$nwords)] <- 0
	}

	cv.text <- unique(cv.text)
	cv.text <- cv.text[order(logit.val,decreasing=T)]
	print(cv.text[is.na(nwords)])
	nwords <- sum(cv.text$nwords)  
	n10 <-round((nwords * compression) , 0)

	print(c(nwords, n10))
	nwords.sum <- 0	
	cv.select <- NULL 
	i <- 1
	while (nwords.sum < n10) {
		cv.select <- rbind(cv.select, cv.text[i])  	
		nwords.sum <- nwords.sum + cv.text[i]$nwords
		i <- i+1
	}
	cv.select <- data.table(cv.select)
	#cv.select <- cv.select[order(starttime)]
	cv.select <- cv.select[order(logit.val, decreasing=T)]
	if (length(curr.feats) < 5) {
		fset <- paste(curr.feats, collapse="-")
	} else {
		fset <- paste(c(curr.feats[1:3], length(curr.feats)), collapse="-")
	}
	outfile <- paste(outdir, "/", currconv, "_", fset, "-", compression,"_", classifier,  ".html", sep="")
	print(outfile)
	write("<html>", file=outfile, append=FALSE) 
	write(paste("<head><title>",currconv,"</title> </head>"), file=outfile, append=T)
	write("<body bgcolor=\"white\">", file=outfile, append=T)
	for (i in 1:nrow(cv.select)) {
		currda <- cv.select[i]$niteid
		anchor <- paste("<a name=", "\"", currda, "\">", sep="") 
		anchor <- paste(anchor, paste("[", currda, "]</a>", sep=""),sep="")	
		anchor <- paste(anchor, paste("<a href=", "\"#", currda, "\" id=",currda, ">", sep="")) 
		anchor <- paste(anchor, cv.select[i]$text,sep="")		
		anchor <- paste(anchor, "</a><br>",sep="")		
		write(anchor, file=outfile, append=T)
	}

	write("</body>", file=outfile, append=T)
	write("</html>", file=outfile, append=T)

	return(cv.select)

}

get.da.text <- function(dafile="../ami.da.word.clean.txt") {
	da.word <- data.table(read.delim(dafile, header=F))
	setnames(da.word, c("wid", "da.id", "word"))
	da.word$word <- tolower(gsub(" ", "", da.word$word))
	da.text <- da.word[,list(nwords=length(wid), text=paste(word, collapse=" ")),by=da.id]
	da.text$text <- gsub("[!.,?]", "", da.text$text)
	da.text$text <- gsub(" $", "", da.text$text)

	da.text <- data.table(conv=da.text[,unlist.df(strsplit(unlevel(da.id), split="\\."))[,1]], da.text)

	return(da.text)
}

write.html.summaries <- function(cv, da.text, outdir, curr.feats, compression=0.10)  {
	print(curr.feats)
	if ("wid" %in% names(cv)) {
		setnames(cv, c("wid"), c("niteid")) 
	}
	setkey(cv, "niteid")
	setkey(da.text, "niteid")
	cv.text.all <- da.text[cv]
	if ("annot" %in% names(cv.text.all)) {
		cv.text.all$annot <- NULL
	}
	cv.text.all <- unique(cv.text.all)
#	print(cv.text.all[is.na(conv) | is.na(conv.1)])

	cv.text.all$nwords[is.na(cv.text.all$conv)] <- 0 
	print(cv.text.all[,unique(conv)])
	if (is.factor(cv.text.all$text)) {
		cv.text.all$text <- unlevel(cv.text.all$text)
	}
	cv.text.all$text[is.na(cv.text.all$conv)] <- ""
	cv.text.all$conv <- cv.text.all$conv.1

	extsum <- dlply(cv.text.all, .(conv), write.html.summary.conv, outdir=outdir, 
				curr.feats=curr.feats,  compression=compression)
	return(extsum)	

}

write.gold.conv <- function(abs.text, outdir) {
	currconv <- unique(abs.text$conv)
	currannot <- unique(abs.text$annot)
	print(c(currconv, currannot))
	if (!is.data.table(abs.text)) {
		abs.text <- data.table(abs.text)
	}
	abs.text <- unique(abs.text[order(starttime), list(da.id,text)])

	outfile <- paste(outdir, "/", currconv, ".", currannot, ".html", sep="")
	print(outfile)

	write("<html>", file=outfile, append=FALSE) 
	write(paste("<head><title>",currconv,"</title> </head>"), file=outfile, append=T)
	write("<body bgcolor=\"white\">", file=outfile, append=T)
	for (i in 1:nrow(abs.text)) {
		currda <- abs.text[i]$da.id
		anchor <- paste("<a name=", "\"", currda, "\">", sep="") 
		anchor <- paste(anchor, paste("[", currda, "]</a>", sep=""),sep="")	
		anchor <- paste(anchor, paste("<a href=", "\"#", currda, "\" id=",currda, ">", sep="")) 
		anchor <- paste(anchor, abs.text[i]$text, sep="")		
		anchor <- paste(anchor, "</a><br>",sep="")		
		write(anchor, file=outfile, append=T)
	}

	write("</body>", file=outfile, append=T)
	write("</html>", file=outfile, append=T)

	return(abs.text)

}

write.conv.das <- function(sum.select, outfile) {
	currconv <- unique(sum.select$conv)
	if ("starttime" %in% names(sum.select)) {
		sum.select <- sum.select[order(starttime)]
	}

	write("<html>", file=outfile, append=FALSE) 
	write(paste("<head><title>",currconv,"</title> </head>"), file=outfile, append=T)
	write("<body bgcolor=\"white\">", file=outfile, append=T)
	for (i in 1:nrow(sum.select)) {
		currda <- sum.select[i]$da.id
		anchor <- paste("<a name=", "\"", currda, "\">", sep="") 
		anchor <- paste(anchor, paste("[", currda, "]</a>", sep=""),sep="")	
		anchor <- paste(anchor, paste("<a href=", "\"#", currda, "\" id=",currda, ">", sep="")) 
		anchor <- paste(anchor, sum.select[i]$text, sep="")		
		anchor <- paste(anchor, "</a><br>",sep="")		
		write(anchor, file=outfile, append=T)
	}
	write("</body>", file=outfile, append=T)
	write("</html>", file=outfile, append=T)
}

write.long.conv <- function(da.text, outdir, compression=0.15) {
	currconv <- unique(da.text$conv)
	print(currconv)
	if (!is.data.table(da.text)) {
		da.text <- data.table(da.text)
	}

	sum.text <- da.text[order(nwords,decreasing=T)]
	nwords <- sum(sum.text$nwords)  
	n10 <-round((nwords * compression) , 0)
	print(c(nwords, n10))
	nwords.sum <- 0	
	sum.select <- NULL 
	i <- 1
	while (nwords.sum < n10) {
		sum.select <- rbind(sum.select, sum.text[i])  	
		nwords.sum <- nwords.sum + sum.text[i]$nwords
		i <- i+1
	}

	outfile <- paste(outdir, "/", currconv, "_long_x.html", sep="")
	print(outfile)
	write.conv.das(sum.select, outfile)

	return(sum.select)

}

write.gold.summaries <- function(abs.ext, da.text, outdir) {
	setkey(abs.ext, niteid)
	setkey(da.text, da.id)
	abs.text <- da.text[abs.ext]
	extsum <- dlply(abs.text, .(conv, annot), write.gold.conv, outdir=outdir) 

	return(extsum)
}

write.long.summaries <- function(da.text, outdir, compression=0.15) {
	extsum <- dlply(da.text, .(conv), write.long.conv, outdir=outdir, compression=compression) 

	return(extsum)
}


eval.feats <- function(fset, group.fx0, test.convs, dev.convs=NULL, 
		outdir="./", moddir="./", 
		corpus=c("ami","icsi")) {

	print("****************eval.feats")
	x.feats <- fset$fset
	fsetname <- fset$fsetname 
	print(fsetname)
	print(x.feats)
	
	if (!is.null(dev.convs)){
		print("train")
		train.set <- group.fx0[!(conv %in% dev.convs)][!(conv %in% test.convs)]
		train.set <- get.zscore.obs(train.set, featnames=x.feats) 

		print("dev")
		dev.set <- group.fx0[(conv %in% dev.convs)]
		dev.set <- get.zscore.obs(dev.set, featnames=x.feats) 
	
		print("test")
		test.set <- group.fx0[(conv %in% test.convs)]
		test.set <- get.zscore.obs(test.set, featnames=x.feats) 
	} else {
		dev.set <- NULL
		train.set <- group.fx0[!(conv %in% test.convs)]
		train.set <- get.zscore.obs(train.set, featnames=x.feats) 
		test.set <- group.fx0[(conv %in% test.convs)]
		test.set <- get.zscore.obs(test.set, featnames=x.feats) 
	}

	dep.var <- "link.eda"
	m <- mod.edag.xfeats(train.set, featnames=x.feats, dep.var=dep.var); display(m$mod)

	cv.dev <- NULL
	if (!is.null(dev.set)) {
		print("dev")
		curr.pred.dev <- get.eda.true.pred(m$mod, dev.set, corpus=F, spk=F)
		cv.dev <- data.table(dev.set[,list(niteid, wid, conv, starttime, endtime, annot,eda.true=link.eda,eda.time)],
								logit.val=curr.pred.dev[,1])
		print(xtable(get.f1(cv.dev)$f1))
		print(round(get.auroc(cv.dev), 3))
	}

	print("test")
	curr.pred <- get.eda.true.pred(m$mod, test.set, corpus=F, spk=F)
	cv <- data.table(test.set[,list(niteid, wid, conv, starttime, endtime, annot,eda.true=link.eda,eda.time)],
							logit.val=curr.pred[,1])
	print(xtable(get.f1(cv)$f1))
	print(round(get.auroc(cv), 3))

	if (is.null(fsetname)) {  
		if (length(x.feats) < 5) {
			fsetname <- paste(x.feats, collapse="-")
		} else {
			fsetname <- paste(c(x.feats[1], x.feats[length(x.feats)], length(x.feats)), collapse="-")
		}
	} 

	## save model
	modfile <- paste(moddir, "/", fsetname, ".lr.mod", sep="")
	save(m, file=modfile)  

	## Save prediction on dev
	if (!is.null(dev.convs)) {
		write.table(cv.dev, file=paste(outdir, "/", fsetname, ".dev.txt", sep=""))
	} 

	## Save prediction on eval 
	write.table(cv, file=paste(outdir, "/", fsetname, ".eval.txt", sep=""))

	## Save everything, why not?
	return(list(cv=cv, cv.dev=cv.dev, m=m, curr.feats=x.feats, fsetname=fsetname))

}


write.fset.summary <- function(x, da.text, compression=0.15, outdir="~/data/ami/derived/summeval/systems/", data.part=c("test","dev")) {
	if (data.part == "test") {  
		extsum <- write.html.summaries(x$cv, da.text, outdir=outdir, curr.feats=x$curr.feats, compression=compression)
	} else if (data.part == "dev") {
		print("dev")
		extsum <- write.html.summaries(x$cv.dev, da.text, outdir=outdir, curr.feats=x$curr.feats, compression=compression)
	} else {
		print("write.fset.summary: unknown data.part")
		extsum <- NULL
	}
	return(extsum)
}


write.abs.conv <- function(abs.text, outdir="~/data/ami/derived/summeval/abstracts/") {
	print(abs.text)
	currconv <- unique(abs.text$conv)
	currannot <- unique(abs.text$annot)
	print(currconv)
	if (!is.data.table(abs.text)) {
		abs.text <- data.table(abs.text)
	}
	outfile <- paste(outdir, "/", currconv, ".", currannot, ".abs.html", sep="")
	print(outfile)
	write.conv.das(abs.text, outfile)

	return(abs.text)
}

write.gold.abstracts <- function(abs.text, outdir="~/data/ami/derived/summeval/abstracts/") {
	extsum <- dlply(abs.text, .(conv, annot), write.abs.conv, outdir=outdir) 
	return(extsum)
}

get.abs.text <- function(afile="../ami.abs.text.txt") {
	abs.text <- data.table(read.delim(afile, header=F))	
	if (ncol(abs.text) == 3) {
		setnames(abs.text, c("da.id","text", "X")) 
		abs.text$X <- NULL
	} else {
		setnames(abs.text, c("da.id","text")) 
	}
	abs.text <- abs.text[!grepl("NA", text)]

	abs.text$text <- gsub("[!.,?]", "", abs.text$text)
	abs.text$text <- gsub(" $", "", abs.text$text)

	abs.text <- data.table(conv=abs.text[,unlist.df(strsplit(unlevel(da.id), split="\\."))[,1]], abs.text)
	abs.text <- data.table(annot=abs.text[,unlist.df(strsplit(unlevel(da.id), split="\\."))[,2]], abs.text)

	return(abs.text)
}
icsi.errors <- function(group.fx0) {
	group.fx0$endtime[group.fx0$niteid=="Bmr021.D.dialogueact710"] <- 1185.15
	da.text$text[da.id=="Bmr021.D.dialogueact710"] <- "hmm"
	da.text$nwords[da.id=="Bmr021.D.dialogueact710"] <- 1 

}


icsi.eda.mods <- function(icsi.eda.da) {

	test.convs <- c("Bed004","Bed009","Bed016","Bmr005","Bmr019","Bro018")
	dev.convs <- c("Bed002", "Bmr013", "Bed008", "Btr001", "Bro008", "Bro028")

	#load("icsi.eda.da")

	group.fx0 <- icsi.eda.da[!is.na(annot)]#[!is.na(mpos)] #[!(niteid %in% nowords$da.id)]
	nrow(group.fx0)

	train.convs <- group.fx0[!(conv %in% test.convs)][!(conv %in% dev.convs), unique(conv)]

	group.fx0 <- fix.group.fx0(group.fx0)
	group.fx0 <- data.table(group.fx0, eda.time=group.fx0$dur)
	group.fx0$eda.time[group.fx0$link.eda == F] <- 0

	align.errors <- group.fx0[, niteid[dur > 30 & (conv %in% train.convs)]]
	group.fx0 <- group.fx0[!(niteid %in% align.errors)]

	save(group.fx0, file="~/data/icsi/derived/icsi.group.fx0.tfsp")

	save(group.fx0, file="~/data/icsi/derived/icsi.group.fx0.tfsp-pros")

	write.table(group.fx0, file="~/data/icsi/derived/icsi.group.fx0.txt")
} 



test.liblinear <- function(train.fx0, test.fx0, pred.vars, dep.var) {
	xTrain <- train.fx0[,pred.vars,with=F]
	yTrain <- train.fx0[[dep.var]]

	xTrain <- scale(xTrain,center=TRUE,scale=TRUE)

	xTest <- test.fx0[,pred.vars, with=F]
	yTest <- test.fx0[[dep.var]]

	xTest <- scale(xTest,center=TRUE,scale=TRUE)
	
	tryCosts <- c(1000,100,10,1,0.1,0.01,0.001)

	ty <- 0 
	bestType <- ty
	bestCost=NA
	bestAcc=0

#	for (ty in c(0:7 ))  {
		for(co in tryCosts){
			acc <- LiblineaR(data=xTrain,labels=yTrain,type=ty,cost=co,bias=TRUE,cross=10,verbose=FALSE)
			cat("Type: :", ty, "\n",sep="")
			cat("Results for C=",co," : ",acc," accuracy.\n",sep="")
			if(acc>bestAcc){
				bestType <- ty
				bestCost <- co
				bestAcc <- acc
			}

		}
#	}
	cat("Best type is:",bestType,"\n")
	cat("Best cost is:",bestCost,"\n")
	cat("Best accuracy is:",bestAcc,"\n")

	# Re-train best model with best cost value.

	#m <- LiblineaR(data=xTrain,labels=yTrain,type=ty,cost=bestCost,bias=TRUE,verbose=T)
	m <- LiblineaR(data=xTrain,labels=yTrain,type=bestType,cost=bestCost,bias=TRUE,verbose=T)

	p <- predict(m,xTest,proba=TRUE,decisionValues=TRUE)

	res <- data.table(logit.val=p$probabilities[,2],eda.true=yTest, niteid=test.fx0[,niteid])
	print(get.f1(res))

	return(list(m=m, cv=res))

}

#################################################################
 
get.ami.test.convs <- function() {
	test.groups <- c("ES2004", "ES2014", "IS1009", "TS3003", "TS3007")
	test.convs <- unlist(lapply(test.groups, function(x) {paste(x, c("a", "b", "c", "d"), sep="")}))
	return(test.convs)
}

get.ami.dev.convs <- function() {
	dev.groups <- c("ES2003", "ES2011", "IS1008", "TS3004", "TS3006")
	dev.convs <- unlist(lapply(dev.groups, function(x) {paste(x, c("a", "b", "c", "d"), sep="")}))
}

## Join in da text, this is really just to write the summaries for ROUGE.
get.da.text <- function(group.fx0, da.word.file, outfile="./da.word.txt", from.nxtql=T) {
	# ~/scripts/ami-da-word-.sh > ami.da.word.txt
	#grep [ABCD].words[0-9] ami.da.word.txt > ami.da.word.clean.txt
	print("=== get.da.text === ")
	if (from.nxtql) {
		da.word <- data.table(read.delim(da.word.file, header=F))
		if (length(names(da.word)) == 4) { ## because NQL gives an extra tab 
			setnames(da.word, c("wid", "da.id", "word", "X"))
			da.words$X <- NULL	
		} else {
			setnames(da.word, c("wid", "da.id", "word"))
		}
	} else { ## It should be a dump of the json word.dt
		da.word <- data.table(read.table(da.word.file, header=T))
		#setnames(da.word, c("word.id", "seg.id", "wordId"), c("wid", "da.id", "word"))		
		setnames(da.word, c("niteid", "seg.id", "word"), c("wid", "da.id", "word"))		
	}

	# Remove extra spaces, punctuation etc and join words into DA strings
	da.word$word <- tolower(gsub(" ", "", da.word$word))
	da.text <- da.word[,list(nwords=length(wid), text=paste(word, collapse=" ")),by=da.id]
	da.text$text <- gsub("[!.,?]", "", da.text$text)
	da.text$text <- gsub(" $", "", da.text$text)
	da.text$text <- gsub("  *", " ", da.text$text)

	## recount the number of words
	v <- strsplit(da.text$text, split=" ")
	da.text$nwords=unlist(lapply(v, length))

	## Join in times from DA feature set  
	da.times <- unique(group.fx0[,list(niteid, conv, starttime, endtime)])
	print(da.times)

	setkey(da.times, niteid)
	setkey(da.text, da.id)
	da.text <- da.times[da.text]
	print(da.text)

	#da.text <- data.table(conv=da.text[,lapply(strsplit(niteid,split="\\."), function(x){x[1]})], da.text)

	print("== save da.text === ")
	print(outfile)
	write.table(da.text, file=outfile)
	#save(da.text, file="~/data/ami/derived/ami.da.text")

	return(da.text)

}

## Write the gold standard summaries
write.ami.summaries <- function(datadir="~/data/ami/derived/", corpus=c("ami", "icsi")) {
	abs.names <- c("abs.id", "s.id", "slink.id", "niteid", "starttime","endtime","da.type")
	ami.abs <- get.abs.ext.dt(filename=paste(datadir, "/", corpus, ".abs.da.txt", sep=""), abs.names=abs.names)

	golddir <- paste(datadir, "/summeval/models/", sep="")	
	write.gold.summaries(abs.ext=ami.abs, da.text=da.text, outdir=golddir)
	sysdir <- paste(datadir, "/summeval/systems/", sep="")	
	write.long.summaries(da.text, outdir=sysdir, compression=0.15)

	abs.text <- get.abs.text("~/data/ami/derived/ami.abs.text.txt")
	abstextfile <- paste(datadir, "/", corpus, "abs.text.txt", sep="")
	abs.text <- get.abs.text(abstextfile)
	absdir <- paste(datadir, "/summeval/abstracts/", sep="")	
	write.gold.abstracts(abs.text, outdir="~/data/ami/derived/summeval/abstracts/")
}


## A little preprocessing, join in DA text. 
prep.data <- function(filename="~/data/ami/derived/da-feats/ami.group.fx0.wsw",  da.word.file=NULL) {

	print("here")	
	x <- load(filename, verbose=T)
	group.fx0 <- get(x)

	print("groupfx0")	
	print(names(group.fx0))
	if (!("starttime" %in% names(group.fx0))) {
		print("wstart to starttime")
		setnames(group.fx0, c("wstart", "wend"), c("starttime", "endtime"))
	}

	#print(group.fx0)

	if (!is.null(da.word.file)) {
	        if (grepl("da.word.clean.txt", da.word.file)) {
			from.nxtql <- T
		} else {
			from.nxtql <- F
		}
		print(da.word.file)

		da.text <- get.da.text(group.fx0, da.word.file=da.word.file, 
			outfile=paste(da.word.file, ".text", sep=""), from.nxtql=from.nxtql)
		setkey(da.text, niteid)
		setkey(group.fx0, niteid)
		group.fx0 <- da.text[group.fx0]
		group.fx0$nwords[is.na(group.fx0$nwords)] <- 0
	}

	## Add in generic group level indicators for if missing 
	if (!("mtype" %in% names(group.fx0))) {
		group.fx0 <- data.table(mtype="none", group.fx0) 
	}

	if (!("mgroup" %in% names(group.fx0))) {
		group.fx0 <- data.table(mgroup="none", group.fx0) 
	}

	if (!("annot" %in% names(group.fx0))) {
		group.fx0 <- data.table(annot="none", group.fx0) 
	}

	if (!("eda.true" %in% names(group.fx0))) {
		group.fx0 <- data.table(eda.true=F, group.fx0) 
	}

	if (!("eda.time" %in% names(group.fx0))) {
		group.fx0 <- data.table(eda.time=0, group.fx0) 
	}

	if (!("link.eda" %in% names(group.fx0))) {
		group.fx0 <- data.table(link.eda=F, group.fx0) 
	}

	if (!("uninterrupted" %in% names(group.fx0))) {
		group.fx0 <- data.table(uninterrupted=group.fx0$dur, group.fx0) 
	}
	

	return(group.fx0)
}

## Get LR models for feature sets in the list fset based on feature set group.fx0  
get.fset.mods <- function(group.fx0, fset, fsetname, write.summary=F,  
		test.convs, dev.convs, 
		evaldir="~/data/ami/derived/segs/reval/", 
		moddir="~/data/ami/derived/mods/", 
		sevaldir="~/data/ami/derived/summeval/", 
		corpus="ami")
{

	## Get feature set
	print("fsetname")
	print(fsetname)
	print("fset")
	print(fset)

	## Train models and test on dev and test sets for each of the feature sets in fset
	## Could parallelize here with mclapply?
	print("get eval.feats")


	fsets <- lapply(fset, function(x) {x$fset})
	print(fsets)

	x.dev <- lapply(fset, eval.feats, group.fx0=group.fx0, test.convs=test.convs, dev.convs=dev.convs, 
			outdir=evaldir, moddir=moddir, 
			corpus=corpus)

	fsetnames <- unlist(lapply(fset, function(x) {x$fsetname}))
	print(fsetnames)
	names(x.dev) <- fsetnames
	save(x.dev, file=paste(moddir, "/", corpus, ".", fsetname, ".mod", sep=""))

	print("write stats")
	x.stats <- data.table(ldply(x.dev, function(x) {data.table(get.f1(x$cv)$f1[1,], auroc=get.auroc(x$cv))}))
	print(paste(evaldir, "/", corpus, ".", fsetname, ".eval.stats.txt", sep=""))
	print(x.stats)
	write.table(x.stats, file=paste(evaldir, "/", corpus, ".", fsetname, ".eval.stats.txt", sep=""))

	print(paste(evaldir, "/", corpus, ".", fsetname, ".dev.stats.txt", sep=""))
	x.dev.stats <- data.table(ldply(x.dev, function(x) {data.table(get.f1(x$cv.dev)$f1[1,], auroc=get.auroc(x$cv))}))
	print(x.dev.stats)
	write.table(x.dev.stats, file=paste(evaldir, "/", corpus, ".", fsetname, ".dev.stats.txt", sep=""))

	## Write ROUGE style summaries, if you want.
	## We do it quicker with another function later...?
#	if (write.summary) {
#		feat.sums <- lapply(x.dev, write.fset.summary, da.text=da.text, compression=0.15, data.part="dev", 
#				outdir=paste(sevaldir, "/devsystems/", sep=""))
#		feat.sums <- lapply(x.dev, write.fset.summary, da.text=da.text, compression=0.15, data.part="test", 
#				outdir=paste(sevaldir, "/systems", sep=""))
#	}
}

apply.fset.mods <- function(group.fx0, fsetname,
                evaldir="~/data/ted/derived/segs/reval/",
                moddir="~/data/ted/derived/mods/",
                sevaldir="~/data/ted/derived/summeval/",
                corpus="ted", mod.corpus="ami", dset="conv")
{

        ## load model
        modfile <- paste(moddir, "/", mod.corpus, ".", fsetname, ".mod", sep="")
        print(modfile)
        modobj <- load(modfile)
        x.mods <- get(modobj)


        ## get predictions 
        lapply(x.mods, function(x) {
                print("=== curr.feats ===")
                print(x$curr.feats)
                print("=== model ===")
                m <- x$m
                print(m)
                test.set <- get.zscore.obs(group.fx0, featnames=x$curr.feats)
                if (is.null(test.set)) {
                        print("missing features")
                        print(x$curr.feats)
                        return(NULL)
                }

                curr.pred <- get.eda.true.pred(m$mod, test.set, corpus=F, spk=F)
                #curr.pred <- data.table(test.set[,list(niteid, wid, conv, starttime, endtime, nwords, annot,eda.true=link.eda)],
                curr.pred <- data.table(test.set[,list(niteid, wid, conv, starttime, endtime, annot,eda.true=link.eda)],
                                                        logit.val=curr.pred[,1])

                test.text <- group.fx0[, list(niteid, text)]
                setkey(curr.pred, niteid)
                setkey(test.text, niteid)
                curr.pred <- curr.pred[test.text]

                curr.pred <- curr.pred[order(conv, logit.val, starttime, decreasing=T)]
                predfile <- paste(evaldir, "/", dset, ".", x$fsetname, ".eval.txt", sep="")
                write.table(curr.pred, file=predfile)

                return(curr.pred)

        })
}


conv.conf <- function () {
seg.prob.file="~/data/inevent/derived/segs/reval/ami.group.fx0.aug.wsw/TED0069.tf.pros_pros.eval.txt"
seg.word.file="~/data/inevent/derived/segs/asrword/TED0069.raw.asrword.txt"

seg.prob.file="~/data/inevent/derived/segs/reval/ami.group.fx0.aug.wsw/TED0227.tf.pros_pros.eval.txt"
seg.word.file="~/data/inevent/derived/segs/asrword/TED0227.raw.asrword.txt"
}
seg.prob.file="~/data/inevent/derived/segs/reval/ami.group.fx0.aug.wsw/KLEdjangocon2012003.tf.pros_pros.eval.txt"
seg.word.file="~/data/inevent/derived/segs/asrword/KLEdjangocon2012003.raw.asrword.txt"

conv="TED1090"
seg.prob.file=paste("~/data/inevent/derived/segs/reval/ami.group.fx0.aug.wsw/", conv, ".tf.pros_pros.eval.txt", sep="")
seg.word.file=paste("~/data/inevent/derived/segs/asrword/", conv, ".raw.asrword.txt", sep="")


add.word.confidence <- function(seg.prob.file, seg.word.file) {
	seg.probs <- data.table(read.table(seg.prob.file, header=T))
	seg.probs <- seg.probs[,list(niteid, logit.val)]
	seg.words <- data.table(read.table(seg.word.file, header=T))
	if ("niteid" %in% names(seg.words)) {
		setnames(seg.words, c("niteid"), c("word.id"))
	}
	if ("wordId" %in% names(seg.words)) {
		setnames(seg.words, c("wordId"), c("word"))
	}
	setnames(seg.words, c("seg.id"), c("niteid"))

	setkey(seg.words, niteid)	
	setkey(seg.probs, niteid)

	segs <- seg.probs[seg.words]
	segs <- data.table(segs, conf.words=tolower(segs$word))
	segs$conf.word[segs$wordConfidence < 0.9] <- "."
	segs.conf <- segs[,list(logit.val=unique(logit.val), mean.conf=mean(wordConfidence), nwords=length(conf.word), conf.trans=paste(conf.word, collapse=" ")), by=niteid]
	segs.conf[order(logit.val, decreasing=T)][1:10]

	x <- data.table(seg.words, conf.word=tolower(seg.words$word))
	x$conf.word[x$wordConfidence < 0.9] <- "."
	y <- strsplit(paste(x$conf.word, collapse=" "), "\\.")
	y <- as.data.table(y)
	y <- y[grep("[a-z]", y$V1)]
	



}


## --------------------------------------------------------------------------
## select.quotes: 
## utt.probs: probs assigned to utterances/DAs from the Extractive summarizer 

get.utt.probs <- function() {
	utt.probs <- data.table(read.table("~/data/inevent/derived/segs/reval/ami.group.fx0.aug.wsw/TED0069.tf.pros_pros.eval.txt", header=T))		
	utt.probs <- data.table(read.table("~/data/inevent/derived/segs/reval/ami.group.fx0.aug.wsw/TED1090.tf.pros_pros.eval.txt", header=T))		
	utt.probs <- data.table(read.table("~/data/inevent/derived/segs/reval/ami.group.fx0.aug.wsw/KLEdjangocon2012003.tf.pros_pros.eval.txt", header=T))		
}

select.quotes <- function(utt.probs) {
	qs <- utt.probs[,quantile(logit.val, probs=c(0.25,0.5,0.75, 0.95, 1))]
	qs <- data.table(quantile=gsub("%", "", names(qs)), qs)
	utt.sel <- qs[, utt.probs[logit.val <= qs][order(logit.val, decreasing=T)][1], by=quantile]
}

