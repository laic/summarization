#!/usr/bin/Rscript --vanilla --slave

source("../rscripts/nxt-proc.r")

library(data.table)
library(plyr)
library(reshape2)

#curr.var.names <- c("niteid", "wstart", "tf.idf", "su.idf", "sqrt.idf",
#			"mean.normF0", "sd.normF0", "q2.5.normF0", "q97.5.normF0", 
#			"mean.normI0", "sd.normI0", "q2.5.normI0", "q97.5.normI0") 


no.norm <- c("niteid","wstart","wend","silence")
znorm.by.window <- function(x, var.names, wpercent=0.25) {
	no.norm <- c("niteid","wstart","wend","silence",grep("lbp",var.names,value=T))
	print("znorm")
	zvals <- NULL
	for (var.name in var.names[!(var.names %in% no.norm)]) {
		print(var.name)
		curr.zvals <- znorm.var.by.window(x, var.name, wpercent=wpercent)		
		#print(curr.zvals[1])
		if (is.null(zvals)) {
			zvals <- curr.zvals
		} else {
			setkey(zvals, niteid)
			setkey(curr.zvals, niteid)
			zvals <- zvals[curr.zvals]
		}
	}
	setkey(x, niteid)
	setkey(zvals, niteid)
	return(x[zvals])
}

znorm.var.by.window <- function(x, var.name="mean.normF0", wpercent=0.25) {
        currconv <- unique(x$conv)
	
        n.segs <- nrow(x) 
        wsize <- floor(n.segs * wpercent)

	print(wsize)
	x <- x[order(wstart)]
	setnames(x, c(var.name), c("val"))

	zvals <- NULL
	for (icurr in 1:nrow(x)) {
		z <- NA
		istart <- max(1, icurr-wsize)		
		iend <- min(nrow(x), icurr+wsize)	
		curr <- x[istart:iend,  list(mean.val=mean(val, na.rm=T), sd.val=sd(val, na.rm=T))]
		#print(curr)
                xval <- x[icurr]$val
		#print(xval)
		z <- (xval - curr$mean.val) / (curr$sd.val)
		zvals <- c(zvals, z)
	}

	setnames(x, c("val"), c(var.name))

	zdata <- data.table(niteid=x$niteid, zval=zvals)

	#print(zdata)
	setnames(zdata, c("zval"), c(paste(var.name,".zw",sep="")))

	return(zdata)

}

## Collate the word level features specified by var.names with context if specified.
collate.tf.feats <- function(xprops, var.names, prev.window=0, next.window=0) {
	x0 <- xprops[order(wstart)]
	currconv <- unique(x0$conv)	 
	print(x0)

	## Calculate silence separately
	x <- x0[, var.names[var.names != "silence"], with=F] 
	
	if (!("link.eda" %in% names(x0))) {
		x0 <- data.table(link.eda=FALSE, x0) 
	}
	curr.names0 <- c("link.eda", "niteid")


	for (var.name in var.names[!(var.names %in% c("silence","niteid"))]) {
		x0[[var.name]][is.na(x0[[var.name]])] <- 0
	}

	if ((prev.window + next.window) == 0) {
		return(x0[,c(curr.names0,var.names[var.names != "silence"][var.names != "niteid"]),with=F])
	}
	
	## No context, just return as is. 

	vals <- NULL
	for (icurr in 1:nrow(x)) {
		## Get current window boundaries
		istart <- max(1, icurr-prev.window)		
		iend <- min(nrow(x), icurr+next.window)	
		xcurr <- x[istart:iend]

		## pad out ends with zeros if necessary
		nmissing.prev <- prev.window + 1 - icurr
		nmissing.next <- icurr + next.window - nrow(x)
		if (nmissing.prev > 0 ) {
			missing <- data.table(niteid=paste(currconv, as.character(1:nmissing.prev),sep=""), x[1,2:ncol(x),with=F] * 0)	
			xcurr <- rbind(missing, xcurr)
		} 

		if (nmissing.next > 0 ) {
			missing <- data.table(niteid=paste(currconv, "n", as.character(1:nmissing.next),sep=""), x[1,2:ncol(x),with=F] * 0)	
			xcurr <- rbind(xcurr, missing)
		} 

		## Put current features in long format: (niteid, variable, value), set NA vals to zero. 
		curr.vals <- data.table(melt(xcurr, id.vars=c("niteid"))) 				
		curr.vals$value[is.na(curr.vals$value)] <- 0

		target.vals <- x[icurr] ## Current word

	
		## Get silence stats over context window.	
		## Add to feature set in wide format. 
		if ("silence" %in% var.names) {
			idur <- x0[istart:iend, max(wend,na.rm=T)-min(wstart,na.rm=T)]
			sum.wdur <- x0[istart:iend,sum(wdur)]				
			sil.dur <- idur-sum.wdur
			curr.vals.wide <- c(x0$link.eda[icurr], unlevel(x0$niteid[icurr]), sil.dur, idur, 
						t(curr.vals$value))
						#t(curr.vals$value), t(delta.vals$value))
		} else { 
			curr.vals.wide <- c(x0$link.eda[icurr], unlevel(x0$niteid[icurr]), t(curr.vals$value))
			#curr.vals.wide <- c(x0$link.eda[icurr], unlevel(x0$niteid[icurr]), t(curr.vals$value), t(delta.vals$value))
		}

		## Put some position numbers on the feature names
		## Not centered!
		nwords=prev.window+next.window+1
		curr.vnames <- paste(unlevel(curr.vals$variable), (c(0:(length(curr.vals$variable)-1)) %% nwords), sep=".") 

		## For each feature type (variable) get difference between 
		## context values and current (target) value.
		
		if ((prev.window + next.window) > 0) {
			delta.vals <- curr.vals[,{
				curr.var <- unique(unlevel(variable))  
				curr.tval <- target.vals[[curr.var]][1]		
				if (is.na(curr.tval)) {
					curr.tval <- 0
				}
				list(value=(curr.tval-value))
				},by=variable]	
			delta.vals$variable <- paste("delta",unlevel(delta.vals$variable),sep=".")
			curr.vals.wide <- c(curr.vals.wide, t(delta.vals$value))
			curr.vnames <- c(curr.vnames, paste(delta.vals$variable, (c(0:(length(delta.vals$variable)-1)) %% nwords ), sep=".")) 
		}

		vals <- rbind(vals, curr.vals.wide) 		
	}
	

	## Add niteid, link.eda and add names to data.table
	print(curr.vnames)
	if ("silence" %in% var.names) {
		curr.names <- c(curr.names0, "sil.dur", "idur", curr.vnames) 
	} else {
		curr.names <- c(curr.names0,  curr.vnames) 
	}
	vals <- data.table(vals)
	setnames(vals, curr.names)

	
	return(vals)

}

get.head.vals <- function(featname=c("LBP","head"), templatefile="~/data/ami/derived/lbp.agg.template.txt", x.tf.pros) {
	print("LBP")

	## lbp.tempate includes niteid column
	x.template <- data.table(read.table(file=templatefile, header=T))
	## Assume the only non feature columns are niteid, wstart and wend.
	n.x.cols <- ncol(x.template) - 3 

	spks <- unique(unlevel(x.tf.pros$nxt_agent)) 
	headdir <- paste(xdir, "/",featname, "/", sep="")  

	if (grepl("da", fsuffix)) {
		x.suffix <- paste(".", featname, ".da.txt", sep="")
	} else {
		x.suffix <- paste(".", featname, ".word.txt", sep="")
	}

	hvals <- NULL
	## We're going to have a problem with missing data. 	
	for (currspk in spks) {
		hfile <- paste(headdir, currconv, ".", currspk, x.suffix, sep="")
		print(hfile)
		try(currhead <- data.table(read.table(hfile, header=T)))

		if (!is.data.table(currhead)) {
			currhead <- x.template[0]
		} 

		## Add zero entries for missing data.
		missing.niteid <- setdiff(x.tf.pros[nxt_agent == currspk]$niteid, currhead$niteid)
		if (length(missing.niteid) > 0) {
			print(paste("missing:", length(missing.niteid)))
			missing.niteid <- setdiff(x.tf.pros[nxt_agent == currspk]$niteid, currhead$niteid)
			missing.dt <- x.tf.pros[niteid %in% missing.niteid, list(niteid, wstart, wend)]  

			x.zero <- as.data.table(matrix(0, nrow=length(missing.niteid), ncol=n.x.cols)) 
			misshead <- data.table(niteid=missing.entries, x.zero)  	
		
			print(names(currhead)[names(currhead) != names(misshead)])	
			currhead <- rbind(currhead, misshead)
		}

		hvals <- rbind(hvals, currhead, use.names=T)
	}	

	hvals <- data.table(hvals)
	return(hvals)
}


############################################################################################


args=(commandArgs(TRUE))

if(length(args)==0){
	print("No arguments supplied.")
} else {
	print(args)
	currconv <- args[1]
	featname <- args[2]
	segsdir <- args[3]
	spk.only <- as.logical(args[4])
	prev.window <- as.numeric(args[5])	
	next.window <- as.numeric(args[6])	
	var.id <- args[7]
	include.head <- as.logical(args[8])
	corpus <- args[9] 		
	fsuffix <- args[10]
	curr.var.names <- args[11:length(args)]

	## Define test and dev sets		
	if (corpus == "ami") {
		test.convs <- c("ES2004", "ES2014", "IS1009", "TS3003", "TS3007")
		dev.convs <- c("ES2003", "ES2011", "IS1008", "TS3004", "TS3006")
	} else if (corpus == "icsi") {
		test.convs <- c("Bed004","Bed009","Bed016","Bmr005","Bmr019","Bro018")
		dev.convs <- c("Bed002", "Bmr013", "Bed008", "Btr001", "Bro008", "Bro028") # Randomly sampled

	} else { 
		print(corpus)
		## Everything else is a test?
		test.convs <- c()
		dev.convs <- c()
	}

	## read word level feature files
	tffile <- paste(segsdir, "/", featname, "/", currconv, fsuffix, sep="")		
	print(tffile)
	x.tf.pros <- data.table(read.table(tffile, header=T))	

	## standardize names if necessary
	if ("starttime" %in% names(x.tf.pros) & !("wstart" %in% names(x.tf.pros))) {
		setnames(x.tf.pros, c("starttime", "endtime"), c("wstart","wend"))
	}

	## add word duration
	x.tf.pros <- data.table(wdur=x.tf.pros[,wend-wstart], x.tf.pros)


	## Join in head features
	if (include.head) {
		print("head")
		headdir <- paste(segsdir, "/head/", sep="")  
		headfiles <- list.files(headdir, pattern=currconv, full.names=T) 	
		if (grepl("da", fsuffix)) {
			headfiles <- grep("da.txt", headfiles, value=T)	
		} else {
			headfiles <- grep("word.txt", headfiles, value=T)	
		}

		hmvt <- NULL
		for (hfile in headfiles) {
			print(hfile)
			currhead <- data.table(read.table(hfile, header=T))
			hmvt <- rbind(hmvt, currhead, use.names=T)
		}	
		hmvt <- data.table(hmvt)
		setkey(hmvt, niteid)
		setkey(x.tf.pros, niteid)	
		x.tf.pros <- x.tf.pros[hmvt]
	}
	print(curr.var.names)
	print("LBP?")
	print(sum(grepl("lbp",curr.var.names)))
	if (sum(grepl("lbp",curr.var.names)) > 0)	 {
		print("LBP")
		hmvt <- get.head.vals(featname="LBP", templatefile="~/data/ami/derived/lbp.agg.template.txt", x.tf.pros) 
		lbp.vars <- grep("lbp", names(hmvt), value=T) 

		setkey(hmvt, niteid)
		setkey(x.tf.pros, niteid)	
		x.tf.pros <- x.tf.pros[hmvt]

		# Add LBP var names to curr.var.names
		curr.var.names <- curr.var.names[curr.var.names != "lbp"] 
		curr.var.names <- c(curr.var.names, lbp.vars)  
		print(curr.var.names)
	}


	#}

	## Generate z-scores with a moving window ala Xie 2009
#	zvals <- znorm.by.window(x.tf.pros, curr.var.names) 
##	outfile.zval <- paste(segsdir, "/", featname,"/", currconv, ".", var.id, ".wz.txt", sep="")	
##	print(outfile.zval)
##	write.table(zvals, file=outfile.zval)
#
	## Whether or not to look at context from the current speaker or everyone
	if (spk.only) {
		x.tf.spk.list <- dlply(x.tf.pros, c("nxt_agent"), function(x) {data.table(x)})
	#	zval.list <- dlply(zvals, c("nxt_agent"), function(x) {data.table(x)})
	} else {
		x.tf.spk.list <- dlply(x.tf.pros, c("conv"), function(x) {data.table(x)})
	#	zval.list <- dlply(zvals, c("conv"), function(x) {data.table(x)})
	}

	## Get feature sequences	
	print("collate.tf.feats")
	x.tf <- unlist.df(lapply(x.tf.spk.list, collate.tf.feats, var.names=curr.var.names, prev.window=prev.window, next.window=next.window))

	## Add suffix for windowed z-score features and grab their sequences 
#	zvar.names <- paste(curr.var.names, ".zw", sep="")
#	zvar.names <- gsub("niteid.zw", "niteid", zvar.names)
#	zvar.names <- gsub("wstart.zw", "wstart", zvar.names)
#	zvar.names <- gsub("wend.zw", "wend", zvar.names)
#	zvar.names <- gsub("silence.zw", "silence", zvar.names)
#
#	z.tf <- unlist.df(lapply(zval.list, collate.tf.feats, var.names=zvar.names, prev.window=prev.window, next.window=next.window))

	# Don't calculate delta features if we're setting things to context free.
	if ((prev.window + next.window) ==0) {
		x.tf <- x.tf[,grep("delta", names(x.tf), value=T, invert=T), with=F]
#		z.tf <- z.tf[,grep("delta", names(z.tf), value=T, invert=T), with=F]
	}


	## Put files in their relevant test/dev/train partition

	if (corpus %in% c("ami", "icsi")) {
		test.part <- ""
		mgroup <- substr(currconv, 1, 6)	
		if (mgroup %in% test.convs) {
			test.part <- "test"
		} else if (mgroup %in% dev.convs) {
			test.part <- "dev"
		} else {
			test.part <- "train"
		}
	}  else {
		test.part <- "test"
	}
	print(paste("Partition:", test.part))

	## Write to partitions
	if (spk.only) {
		outfile.txt <- paste(segsdir, "/", test.part, "/", currconv, ".", var.id, "-", prev.window, ".", next.window, sep="")	
		zoutfile.txt <- paste(segsdir, "/", test.part, "/", currconv, ".", var.id, "-", prev.window, ".", next.window, ".zw", sep="")	
	} else {
		outfile.txt <- paste(segsdir, "/", test.part, "/", currconv, ".", var.id, "-", prev.window, ".", next.window, ".all.txt", sep="")
		zoutfile.txt <- paste(segsdir, "/", test.part, "/", currconv, ".", var.id, "-", prev.window, ".", next.window, ".all.zw.txt", sep="")
	}

	print(outfile.txt)
	write.table(x.tf, file=outfile.txt, row.names=F, quote=F)	

#	print(zoutfile.txt)
#	write.table(z.tf, file=zoutfile.txt, row.names=F, quote=F)	

	##  Dump to one directory.
	## outfile.all <- paste(segsdir, "/all/", currconv, ".", var.id, "-", prev.window, ".", next.window, ".all.txt", sep="")		
	##print(outfile.all)
	##write.table(x.tf, file=outfile.all, row.names=F, quote=F)	

#	zoutfile.all <- paste(segsdir, "/all/", currconv, ".", var.id, ".", prev.window, ".", next.window, ".all.zw.txt", sep="")
#	write.table(z.tf, file=zoutfile.all, row.names=F, quote=F)	

}


print("END")

