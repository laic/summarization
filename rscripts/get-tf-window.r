#!/usr/bin/Rscript --vanilla --slave

source("../rscripts/nxt-proc.r")
source("../rscripts/term-freq.r")

args=(commandArgs(TRUE))

if(length(args)==0){
	print("No arguments supplied.")
} else {
	print(args)
	currconv <- args[1]
	featname <- args[2]	## e.g. lex, asrlex
	segsdir <- args[3]
	window.type <- args[4]
	spk.only <- as.logical(args[5])
	featset <- args[6] 	## e.g. tf.pros

	if (length(args) == 7) {
		windowdir <- args[7] 		
	} else {
		windowdir <- paste(segsdir, "/", window.type, "/", sep="")		
	}
	
	## Get windows		
	windowfile <- paste(windowdir, "/", currconv, ".conv.", window.type,".txt", sep="")		
	#print(windowfile)
	#windowobj <- load(windowfile)
	#x.list <- get(windowobj)

	#x.window <- data.table(unlist.df(x.list))
	x.window <- data.table(read.table(windowfile, header=T))
	print(summary(x.window))
	print("HERE")

	## This was supposed to make parallelization easier, but maybe needs rethinking.
	x.wlist <- list() 
	for (i in 1:nrow(x.window)) {
		x.wlist[[i]] <- x.window[i,]
	}
	names(x.wlist) <- x.window$niteid
	print(length(x.wlist))

	## Get feature file 
	print(featset)
	if (grepl(currconv, featset)) {
		tffile <- paste(segsdir, "/", featname, "/", featset,  ".txt", sep="")		
	} else {
		tffile <- paste(segsdir, "/", featname, "/", currconv, ".", featset,  ".txt", sep="")		
	}
	print(tffile)
	x.tf.pros <- data.table(read.table(tffile, header=T))	

	## Get DA/larger window level stats.	
	x.tf.pros.da <- get.tfidf.docs.conv(x.tf.pros, windows=x.wlist, spk.only=spk.only)
	print(x.tf.pros.da[1,])

	fstem <- strsplit(featset, "\\.")[[1]][1]
	if (grepl(currconv, fstem)) {
		outstem <- featset  
	} else {
		outstem <- paste(currconv, featset, sep=".")
	}

	if (spk.only) {
		outfile.txt <- paste(segsdir, "/", featname, "/", outstem, ".", window.type, ".spk.txt", sep="")		
	} else {
		outfile.txt <- paste(segsdir, "/", featname, "/", outstem, ".", window.type, ".txt", sep="")		
	}
	
	
	print(outfile.txt)
	write.table(x.tf.pros.da, file=outfile.txt, row.names=F)	
}

print(warnings())
print("END")

