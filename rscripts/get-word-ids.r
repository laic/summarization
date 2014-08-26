#!/usr/bin/Rscript --vanilla --slave
source("../rscripts/f0basics.r")
library(data.table)
library(plyr)
############################################################################################

merge.conv.features <- function(x, featset, lexdir="~/data/icsi/derived/segs/lex/", suffix=".tf.allpros.txt") {
	print(lexdir)
	currconv <- unique(x$conv) 
	print(currconv)

	if (!is.data.table(x)) {
		x <- data.table(x)
	}
	
	lexinfo <- data.table(read.table(paste(lexdir,"/", currconv, suffix, sep=""), header=T))

	setkey(lexinfo, niteid)
	setkey(x, niteid)
	xprobs <- x[lexinfo]
	xprobs <- data.table(xprobs, log.prob=xprobs[,log(prob)])

	print(paste("featset:", featset))
	featname <- gsub(paste(currconv, ".", sep=""), "", featset)	
	print(paste("featname:", featname))

	setnames(xprobs, c("niteid", "id", "prob", "wstart", "wend", "log.prob"), 
			c("id", "mlp.id", featname, "starttime", "endtime", paste(featname,".log",sep=""))) 


	outfile.txt <- paste(lexdir, "/", currconv, ".", featname, ".txt", sep="") 
	write.table(xprobs, outfile.txt, row.names=F)

}


args=(commandArgs(TRUE))

if(length(args)==0){
	print("No arguments supplied.")
} else {
	print(args)
	probfile <- args[1] 	# segs/nn/x.pkl.eval.txt
	idfname <- args[2]	 
	lexdir <- args[3]

	probfstem <- basename(probfile)
	partsplit  <- strsplit(probfstem, split="\\.")[[1]]
	testpart <- partsplit[(length(partsplit)-1)]
	featset0 <- strsplit(probfstem, split="_")[[1]][3]
	hlevels0 <- strsplit(probfstem, split="_")[[1]][4]
	hlevels <- strsplit(hlevels0, split="\\.")[[1]][1]
	featset <- paste(featset0, hlevels, sep="_")

	print(c(featset, testpart))
	prob <- data.table(read.table(probfile, header=T))

	curr.id <- data.table(read.table(idfname, header=T))
	curr.id <- data.table(id=c(1:nrow(curr.id)), curr.id) ## Add ids
	curr.id <- curr.id[, list(id=id, niteid=niteid)]

	setkey(prob, id)
	setkey(curr.id, id) 
	xdata <- prob[curr.id]

	xdata <- data.table(conv=xdata[,list(conv=strsplit(unlevel(niteid),split="\\.")[[1]][1]),by=id]$conv, xdata) 
	print(xdata[1])
	
	x.list <- dlply(xdata, c("conv"), merge.conv.features, featset=featset, lexdir=lexdir)
}


print("END")

