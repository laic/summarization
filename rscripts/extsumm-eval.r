library(ROCR)
library(data.table)
library(ggplot2)

score.eval.set <- function(sum.link.avg, dirname="~/data/ami/derived/segs/eval/", pattern="zfeat$", id.name="wid") {
	filenames <- list.files(dirname, pattern=pattern)
	irstats <- NULL
	no.eda <- NULL 

	for (filename in filenames) {	
		print(filename)
		xobj <- load(paste(dirname, filename, sep=""))
		x.pred <- get(xobj)
		x.pred$cv <- data.table(conv=x.pred$cv[,substr(wid,1,7)], annot="lmer", x.pred$cv[,list(wid,eda.true,logit.val)])
		x <- x.pred$cv[logit.val > 0.5]
		if (nrow(x) > 0) {

			xauroc <- data.table(ddply(x.pred$cv, c("conv","annot"), get.auroc), 
						all.auroc=get.auroc(x.pred$cv))
			setnames(xauroc, c("V1"), c("conv.auroc"))

			nw.stats <- data.table(ddply(x.pred$cv, c("conv","annot"), function(x) {get.f1(x)$f1[eda.true==F]})) 
			nw.all <- get.f1(x.pred$cv)$f1[eda.true==F]	

			nw.stats <- data.table(nw.stats[, list(conv, annot, conv.baseline=V1, 
				conv.acc=accuracy, conv.precision=precision, conv.recall=recall, conv.F1=F1)], nw.all)
			currstats.w <- data.table(fname=gsub(".eval.pred","",filename), 
					weighted.F(x, sum.link.avg))

			setkey(currstats.w, conv, annot)
			setkey(nw.stats, conv, annot)
			print("here")
			setkey(xauroc, conv, annot)
			currstats <- currstats.w[nw.stats][xauroc]
			currstats$fname[is.na(currstats$fname)] <- gsub(".eval.pred","",filename) 
			currstats$w.precision[is.na(currstats$w.precision)] <- 0
			currstats$w.recall[is.na(currstats$w.recall)] <- 0
			currstats$w.f1[is.na(currstats$w.f1)] <- 0

			print(currstats[1])
			irstats <- rbind(irstats, currstats) 
			#print(irstats[is.na(fname)])
		} else {
			print(paste("no eda:", filename))
		}

		

	}

	return(irstats)
}

get.auroc <- function(y) {
	y <- data.table(y)
	if(nrow(y[eda.true==T]) == 0) {		
		return(0)
	}
	rocpred <- prediction(y$logit.val, y$eda.true)
	xauc <- attributes(performance(rocpred, "auc"))$y.values[[1]]
	return(xauc)

}


weighted.precision <- function(x, sum.link.avg, id.name="wid") {

	setnames(x, c(id.name), c("niteid"))
	u <- sum.link.avg[niteid %in% intersect(niteid, x$niteid)][, list(sum.da.avg=sum(da.avg)), by=conv] 	
	v <- x[,list(n.eda=length(niteid)),by=list(conv,annot)]
	setnames(x, c("niteid"), c(id.name))
	
	setkey(v, conv, annot)
	setkey(u, conv)

	if (nrow(u) > 0){
		wprec <- v[u][,list(w.precision=sum.da.avg/n.eda), by=list(conv,annot)]
	} else {
		print("here")
		wprec <- data.table(conv=unique(x$conv), annot=unique(x$annot), w.precision=0)	
	}	

	return(wprec)
} 

weighted.recall <- function(x, sum.link.avg, id.name="wid") {

	setnames(x, c(id.name), c("niteid"))
	u <- sum.link.avg[niteid %in% intersect(niteid, x$niteid)]
	setkey(x, niteid, conv, annot)
	setkey(u, niteid, conv)
	u <- x[u][, list(sum.da.avg=sum(da.avg)), by=list(conv,annot, n.links.conv)] 	
	setnames(x, c("niteid"), c(id.name))

	if (nrow(u) > 0) {	
		wrecall <- u[,list(w.recall=sum.da.avg/n.links.conv), by=list(conv,annot)]
	} else {
		wrecall <- data.table(conv=unique(x$conv), annot=unique(x$annot), w.recall=0)	
	}
	return(wrecall)

}

weighted.F <- function(x, sum.link.avg, id.name="wid") {
	wprec <- weighted.precision(x,sum.link.avg,id.name=id.name)
	wrecall <- weighted.recall(x,sum.link.avg,id.name=id.name)
	setkey(wprec, conv, annot)
	setkey(wrecall, conv, annot)
	
	wstats <- wprec[wrecall]
	wstats <- wstats[, list(w.precision, w.recall, 
			w.f1=(2*w.precision*w.recall)/(w.precision+w.recall)),
		by=list(conv, annot)]


	y <- unique(x[,list(conv,annot)])
	setkey(y, conv, annot)
	setkey(wstats, conv, annot)
	wstats <- wstats[y]

	return(wstats)
}  


get.sum.link.avg <- function(abs.ext) {

	sum.link.avg <- abs.ext[,list(conv=unique(conv), n.annot=unique(n.annot), n.links=length(slink.id), 
		da.avg=length(slink.id)/unique(n.annot)),by=list(niteid)]
	u <- sum.link.avg[,list(n.links.conv=sum(n.links)),by=conv]
	setkey(u, conv)
	setkey(sum.link.avg, conv)
	return(u[sum.link.avg])

}


add.eval.ids <- function(eval.ids, dirname="~/data/ami/derived/mlp/plain/", outdir="~/data/ami/derived/segs/nn_eval/", prefix="") {
	#fnames <- list.files(dirname, pattern="*.txt") 
	fnames <- list.files(dirname, pattern=paste(prefix, ".*.txt", sep=""))
	setkey(eval.ids, evalid)
	for (fname in fnames) {
		print(fname)
		fstem <- gsub(".pkl.txt", "", fname)
		params <- strsplit(fstem, split="-")[[1]]  
		u <- data.table(read.table(paste(dirname, fname, sep="" ), header=T)) 

		setnames(u, c("id","y","pred","prob"), c("evalid","eda.true","eda.pred","logit.val"))
		setkey(u, evalid)		

		curr <- data.table(fstem=fstem, classifier=params[2], lex=params[3], 
				eval.ids[u])
		outfile <- paste(outdir, "/", fstem, ".txt", sep="")
		if (prefix == "murray") {
			setnames(curr, c("niteid", "link.eda", "eda.pred"), c("da.id", "da.eda", "da.pred"))
			outfile <- paste(outdir, "/", fstem, ".da.txt", sep="")
		} 

		print(outfile)
		write.table(curr, file=outfile, row.names=F)
	}

	return(0)
}


add.da.id <- function(da.word, eval.dir="~/data/ami/derived/segs/nn_eval/") {
	setkey(da.word, niteid)
	fnames <- list.files(eval.dir, pattern="[0-9].txt") 
	for (fname in fnames) {
		print(fname)
		fstem <- gsub(".txt", "", fname)
		u <- data.table(read.table(paste(eval.dir, fname, sep="" ), header=T)) 

		setkey(u, niteid)		
		curr <- da.word[u]

		curr <- curr[!is.na(da.id)]  
		da.link.eda  <- curr[,list(da.eda=unique(link.eda), n.word.pred=sum(eda.pred), da.pred=(sum(eda.pred)>0)),da.id]
		setkey(da.link.eda, da.id)
		setkey(curr, da.id)	
		
		curr <- da.link.eda[curr]
	
		outfile <- paste(eval.dir, "/", fstem, ".da.txt", sep="")
		print(outfile)
		write.table(curr, file=outfile, row.names=F)
	}

	return(0)
}


get.eda.words <- function(eda.dir="~/data/ami/derived/segs/extractive") {
	fnames <- list.files(dirname, pattern="multi") 
	xs <- NULL
	for (fname in fnames) {
		print(fname)
		u <- data.table(read.table(paste(dirname, fname, sep="" ), header=T)) 
		xs <- rbind(xs, u)
	}

	return(xs)
}

get.dir.auroc <- function(dirname="~/data/ami/derived/mlp/plain/" ) {
	fnames <- list.files(dirname, pattern="*.dev.txt") 
	xauroc <- NULL
	for (fname in fnames) {
		print(fname)
		fstem <- gsub(".pkl.dev.txt", "", fname)
		params <- strsplit(fstem, split="_")[[1]]  
		u <- data.table(read.table(paste(dirname, fname, sep="" ), header=T)) 
		setnames(u, c("id","y","pred","prob"), c("id","eda.true","eda.pred","logit.val"))
		curr <- data.table(fstem=fstem, classifier=params[2], lex=params[3], 
				get.auroc(u)) 
		xauroc <- rbind(xauroc, curr)
	}

	return(xauroc)
}

get.classifier.auroc <- function(fname, dirname="~/data/ami/derived/segs/nn_eval/", abs.ext=NULL, da.rm=c("x"), ...) {
	print(fname)
	fstem <- gsub(".da.txt", "", fname)
	u <- data.table(read.table(paste(dirname, "/", fname, sep="" ), header=T)) 
	curr.f1 <- get.f1.noprob(u, dep.var="eda.true", pred.var="eda.pred", id.var="niteid")$f1
	curr.auroc <- get.auroc(u)
	curr <- data.table(fstem=fstem, curr.f1[eda.true==F], auroc=curr.auroc) 

	pred <- prediction(u$logit.val, u$link.eda)
	perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
	plot(perf,...)

	return(list(irstats=curr, perf=perf))
}

get.da.irstats <- function(abs.ext=NULL, dirname="~/data/ami/derived/segs/nn_eval/", da.rm=c("x"), pattern=".eval.txt") {
	fnames <- list.files(dirname, pattern=pattern) 
	xstats <- NULL
	for (fname in fnames) {
		print(fname)
		fstem <- gsub(pattern, "", fname)
		u <- data.table(read.table(paste(dirname, "/", fname, sep="" ), header=T)) 
		v <- unique(u[,list(da.id, da.eda, da.pred)])
		if (!is.null(abs.ext)) {
			v <- v[,list(da.eda=da.id %in% abs.ext$da.id, da.eda.word=da.eda, da.pred=da.pred, da.id)]
		}
		v <- v[!(da.id %in% da.rm)] 

		f1 <- get.f1.noprob(v, dep.var="da.eda", pred.var="da.pred", id.var="da.id")$f1
		curr <- data.table(fstem=fstem, f1[eda.true==F]) 
		xstats <- rbind(xstats, curr)
	}

	return(xstats)
}

get.eval.wids <- function(dirname="~/data/ami/derived/segs/nn/", prefix="", header=F) {
	fnames <- list.files(dirname, pattern=paste(prefix, ".*-eval", sep=""))
	xs <- NULL
	## This is really just for checking that we don't have any misalignment 
	for (fname in fnames)  {
		print(fname)
		u <- data.table(read.table(paste(dirname, fname, sep="" ), header=header)) 
		if (header==T) {
			u <- data.table(fname=fname, id=1:nrow(u), u[,list(eda.true, niteid)])	
		} else {
			u <- data.table(fname=fname, id=1:nrow(u), u[,list(eda.true=V1, niteid=V2)])	
		}
		xs <- rbind(xs, u)
	}

	eval.ids <- xs[,list(evalid=unique(id), link.eda=unique(eda.true)),by=list(niteid)]	
	return(eval.ids)

}

get.f1.noprob <- function(x, dep.var, pred.var, id.var) {
	if (!is.data.table(x)) {
		x <- data.table(x)
	}
	setnames(x, c(id.var), c("wid"))
	setnames(x, c(dep.var), c("eda.true"))
	setnames(x, c(pred.var), c("eda.pred"))
	
	
	ctab <- x[,length(wid),by=list(eda.true, eda.pred)]
	baseline <- x[,length(wid)/nrow(x),by=list(eda.true)]
	accuracy <- ctab[,sum(V1[eda.true == eda.pred])/(sum(V1))]
	precision <- ctab[,V1[eda.true & eda.pred]/(sum(V1[eda.pred]))]
	if (length(precision)==0) {precision <- 0}
	recall <- ctab[,V1[eda.true & eda.pred]/(sum(V1[eda.true]))]
	if (length(recall)==0) {recall <- 0}
	F1 <- 2* ((precision*recall)/(precision+recall))

	setnames(x, c("wid"), c(id.var))
	setnames(x, c("eda.true"), c(dep.var))
	setnames(x, c("eda.pred"), c(pred.var))

	return(list(f1=data.table(baseline, accuracy, precision=precision, recall=recall, F1=F1), ctab=ctab))

}

plot.roc <- function(u, ...) {
	pred <- prediction(u$logit.val, u$eda.true)
	perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
	plot(perf,...)
}

read.rouge.results <- function(rfile="~/data/ami/derived/summeval/ami.rouge.abs.300s.txt") {
	x <- data.table(read.table(rfile, header=F))
	setnames(x, c("fset", "rouge", "metric", "val", "v5", "ci.low", "v7", "ci.high"))
	x$ci.high <- as.numeric(gsub(")","",x$ci.high))
	x$metric <- gsub("Average_","",x$metric)
	x$metric <- gsub(":","",x$metric)
	x$fset <-gsub("mean.normF0-q97.5.normF0-sd.normF0-mean.normI0-q97.5.normI0-time.from.prevs-time.to.nexts-spk.rate", "prosody", x$fset)
	x$fset <-gsub("dur.1-uninterrupted-doc.len", "len", x$fset)
	x$fset <-gsub("doc.len-dur.1-uninterrupted", "len", x$fset)
	x$fset <- gsub("spk.dom.prop-spk.dom.da", "dom", x$fset)
	x$fset <- gsub("trank-mpos", "struct", x$fset)
	x$fset <- gsub("mean.overall.*vertical", "head.all", x$fset)
	x$fset <- gsub("tf.idf-su.idf", "lex", x$fset)
	x$fset <- gsub("prosody-len-dom-struct-lex", "murray.all", x$fset)
	x$fset <- gsub("prosody-0.15", "murray.prosody", x$fset)
	x$fset <- gsub("prosody-0.15", "murray.prosody", x$fset)
	x$fset <- gsub("-0.15$", "", x$fset)
	return(x[,list(fset, rouge, metric, val, ci.low, ci.high)])
}

plot.rouge.res <- function(x, rval="ROUGE-1") {
# x <- abs.300s
	feats <- c("su.sdif.14.norm", "tf.sdif.14.norm", "su.idf" , "tf.idf",
		"if.0" ,
		"tf.dur.if.0", "tf.sil.dur.if.14", 
		"su.dur.if.0", "su.sil.dur.if.14", 
		"long", "len", "murray.all", "struct", "prosody", "dom", "lex")
	

	v <- x[fset %in% feats][rouge==rval][metric=="F"]
	vlevels <- v[order(val)]$fset
	v$fset <- factor(v$fset, levels=vlevels)

	p <- ggplot(v, aes(x=fset, y=val)) + geom_pointrange(aes(ymin=ci.low, ymax=ci.high))
        p <- p + theme(axis.text.x = element_text(angle=90, hjust=1, size=14)) 
        p <- p + theme(axis.text.y = element_text(size=14)) 
	p <- p + scale_x_discrete(name="")
	p <- p + scale_y_continuous(name="Average F")
	p <- p + ggtitle(paste(rval, ": Average F, 300 word abstracts", sep=""))
	p


	#ggsave("~/InEvent/ami/ami/lex/fig/ami-r1-300s.png")
}

main <- function() {

	eda.dt <- get.eda.dt("../ami.eda.allannot.txt", annot.index=3)


	
	#load("ami.das")
	#ami.n.das <- ami.das[,list(n.das=length(niteid)),by=conv]
	#setkey(ami.n.das, conv)

	abs.ext <- get.abs.ext.dt()
	sum.link.avg <- get.sum.link.avg(abs.ext)
	save(sum.link.avg, file="ami.sum.link.avg")


	ami.zfeat.eval <- score.eval.set(sum.link.avg,dirname="~/data/ami/derived/segs/eval/", pattern="zfeat.link.eda$", id.name="wid")
	m.name <- unique(ami.zfeat.eval[grep("^mean", fname), fname])
	
	ami.zfeat.lin.eval <- score.eval.set(sum.link.avg,dirname="~/data/ami/derived/segs/eval/", pattern="zfeat.link.eda.lin$", id.name="wid")

	ami.rawfeat.eval <- score.eval.set(sum.link.avg,dirname="~/data/ami/derived/segs/eval/", pattern="rawfeat$", id.name="wid")


	x <- eda.dt[conv=="ES2002a"][annot=="dharshi"]

	dirname <- "~/data/ami/derived/segs/extractive/"

	png("fig/ami-mlp-tf.png")
	u <- get.classifier.auroc("ami-lr-tfidf-0-1.da.txt", col="black", lty=1)
	u <- get.classifier.auroc("ami-mlp-tf-if-0-9-9.da.txt", col="red", lty=1, add=T)
	u <- get.classifier.auroc("ami-mlp-tf-dur-if-0-10-10.da.txt", col="blue", lty=1, add=T)
	u <- get.classifier.auroc("ami-mlp-tf-sil-dur-if-14-152-152.da.txt", col="green", lty=2, add=T)
	u <- get.classifier.auroc("ami-mlp-tf-if-14-135-135.da.txt", col="red", lty=2, add=T)
	u <- get.classifier.auroc("ami-mlp-tf-dur-if-14-150-150.da.txt", col="blue", lty=2, add=T)
	u <- get.classifier.auroc("ami-mlp-tf-14-15-15.da.txt", col="black", lty=2, add=T)
	title(main="AMI MLP: word level EDA prediction with augmented tf")
	legend("bottomright", legend=c("tf", "tf-pros", "tf-pros-dur", "tf-pros-dur-sil"), col=c("black","red","blue","green"), title="features", bty="n", pch=1)
	legend("topleft", legend=c("1", "15"), lty=c(1,2), title="window size", bty="n")
	dev.off()

	png("fig/ami-mlp-su.png")
	u <- get.classifier.auroc("ami-lr-su-0-1.da.txt", col="black", lty=1)
	u <- get.classifier.auroc("ami-mlp-su-if-0-9-9.da.txt", col="red", lty=1, add=T)
	u <- get.classifier.auroc("ami-mlp-su-dur-if-0-10-10.da.txt", col="blue", lty=1, add=T)
	u <- get.classifier.auroc("ami-mlp-su-sil-dur-if-14-152-152.da.txt", col="green", lty=2, add=T)
	u <- get.classifier.auroc("ami-mlp-su-if-14-135-135.da.txt", col="red", lty=2, add=T)
	u <- get.classifier.auroc("ami-mlp-su-dur-if-14-150-150.da.txt", col="blue", lty=2, add=T)
	u <- get.classifier.auroc("ami-mlp-su-14-15-15.da.txt", col="black", lty=2, add=T)
	title(main="AMI MLP: word level EDA prediction with augmented su")
	legend("bottomright", legend=c("su", "su-pros", "su-pros-dur", "su-pros-dur-sil"), col=c("black","red","blue","green"), title="features", bty="n", pch=1)
	legend("topleft", legend=c("1", "15"), lty=c(1,2), title="window size", bty="n")
	dev.off()


}
