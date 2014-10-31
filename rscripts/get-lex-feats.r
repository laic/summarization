#!/usr/bin/Rscript --vanilla --slave

## Collate a big feature data.table, group.fx0, which includes lexical and
## prosodic features over da/utt segments. 
 
source("../rscripts/f0basics.r")
source("../rscripts/nxt-proc.r")

get.ami.test.convs <- function() {
        test.groups <- c("ES2004", "ES2014", "IS1009", "TS3003", "TS3007")
        test.convs <- unlist(lapply(test.groups, function(x) {paste(x, c("a", "b", "c", "d"), sep="")}))
        return(test.convs)
}

get.ami.dev.convs <- function() {
        dev.groups <- c("ES2003", "ES2011", "IS1008", "TS3004", "TS3006")
        dev.convs <- unlist(lapply(dev.groups, function(x) {paste(x, c("a", "b", "c", "d"), sep="")}))
}

get.icsi.test.convs <- function() {
        c("Bed004","Bed009","Bed016","Bmr005","Bmr019","Bro018")
}

get.icsi.dev.convs <- function() {	
        c("Bed002", "Bmr013", "Bed008", "Btr001", "Bro008", "Bro028")
}


## collate.tf.pros: Read in different data sets and join by niteid
collate.tf.pros <- function(fsets, fdir, suffix, prefix="") {
	print("--- collate.tf.pros ---")
	all.feats <- NULL
	print(fsets)
	for (fset in fsets)  {
		currpattern <- paste(prefix, "*\\.", fset, suffix, sep="")
		
		curr.feats <- read.lex.pros(prosdir=fdir, pattern=currpattern)
			
		if (!is.null(curr.feats)) {
			print(paste("have data for:", currpattern))
			if (!("niteid" %in% names(curr.feats))) {
				setnames(curr.feats, c("id"), c("niteid"))
			}

			if ("conv" %in% names(curr.feats)) {
				curr.feats <- curr.feats[!is.na(conv)]
			}

			# Because R does this substitution somewhere else
			if (!is.null(all.feats)) {
				fsetvals <- grep(gsub("-", ".", fset), names(curr.feats), value=T)
				curr.feats <- curr.feats[,c("niteid", fsetvals), with=F]
			}

			## Join in
			setkey(curr.feats, niteid)
			print(nrow(curr.feats))	
			if (!is.null(all.feats)) {	
				setkey(all.feats, niteid)
				all.feats <- curr.feats[all.feats]
			} else {	
				all.feats <- curr.feats
			}
			print(nrow(all.feats))	
			print(nrow(curr.feats))	
		}

	}
	return(all.feats)
}

## Read in files of lexical and prosodic features
read.lex.pros <- function(prosdir="/disk/scratch/ami/derived/segs/f0", pattern="*.aggs.word$", add.name=F) {
	print("--- read.lex.pros ---")
        x.lex.pros <- NULL
	print(pattern)
	print(prosdir)	
        filenames <- list.files(prosdir, pattern=pattern)
	if (length(filenames) == 0) {
		print(paste("No files with pattern", pattern)) 
		return(NULL)
	}
        print(filenames)
        for (filename in filenames) {
                curr <- data.table(read.table(paste(prosdir,filename,sep="/"), header=TRUE))
		setnames(curr, names(curr), gsub("delta.","d.", names(curr)))  	
                if(add.name) {
                        curr <- data.table(fname=filename, curr)
                }
                x.lex.pros <- rbind(x.lex.pros, curr)
        }

        return(data.table(x.lex.pros))
}

## Group together lexical and prosodic features, with augmented features determined by 
## lexsets.
get.feats.main <- function(corpus=c("ami", "icsi", "ted"), datadir="~/data/ami/derived/", 
		lexsets=c("tf-if-slope-0.0.all_9-72", "tfsp-if-slope-0.0.all_12-96","if-0.0.all_8-72"), 
		pscores=F, head=F, outfile="ami.group.fx0.wsw", 
		manual.eda=T, lextype="lex", lexsuffix=".da.spk.txt", prefix="", wtype="asrutt")
{
	lexdir <- paste(datadir, "/segs/", lextype, "/", sep="")
	
	fsets <- lexsets
	print(fsets)

	print("== lex ===")
	xcorpus.tf.pros.da <- collate.tf.pros(fsets, fdir=lexdir,  suffix=lexsuffix, prefix=prefix)
	nrow(xcorpus.tf.pros.da)
	print(names(xcorpus.tf.pros.da))

	print("== f0 ==")
	fsets <- c("f0")
	print(fsets)
	prosdir <- paste(datadir, "/segs/f0/", sep="")
	xcorpus.f0 <- collate.tf.pros(fsets, fdir=prosdir, suffix=paste("-", wtype, ".4.4.all.txt", sep=""), prefix=prefix)
	setkey(xcorpus.f0, niteid)

	print("== intensity ===")
	fsets <- c("i0")
	print(fsets)
	prosdir <- paste(datadir, "/segs/i0/", sep="")
	xcorpus.i0 <- collate.tf.pros(fsets, fdir=prosdir, suffix=paste("-", wtype, ".4.4.all.txt", sep=""), prefix=prefix)
	xcorpus.i0 <- xcorpus.i0[,grep("wstart", names(xcorpus.i0), invert=T, value=T), with=F]
	xcorpus.i0 <- xcorpus.i0[,grep("wend", names(xcorpus.i0), invert=T, value=T), with=F]
	setkey(xcorpus.i0, niteid)
	print("=====")

	xcorpus.if <- xcorpus.f0[xcorpus.i0]
	setkey(xcorpus.if, niteid)	

	if (head==T) {
		headir <- paste(datadir, "/segs/head/", sep="")
		fsets <- c("hmvt")
		xcorpus.head <- collate.tf.pros(fsets, fdir=headdir, suffix=".da.txt", prefix=prefix)
		xcorpus.head <- xcorpus.head[,grep("wstart", names(xcorpus.head), invert=T, value=T), with=F]
		xcorpus.head <- xcorpus.head[,grep("wend", names(xcorpus.head), invert=T, value=T), with=F]
		setkey(xcorpus.head, niteid)

		xcorpus.if <- xcorpus.head[xcorpus.if]
		setkey(xcorpus.if, niteid)	
	}

	if (pscores == T) {
		dadir <- paste(datadir, "/segs/da/", sep="")
		#xcorpus.pscores.da.0 <- read.lex.pros(prosdir="~/data/xcorpus/derived/segs/da/", pattern="*.pscores.da.0.txt")
		xcorpus.pscores.da.10 <- read.lex.pros(prosdir=dadir, pattern=paste(prefix, ".*.pscores.da.10.txt", sep=""))
		#setnames(xcorpus.pscores.da.10, names(xcorpus.pscores.da.10), gsub("$", ".10", names(xcorpus.pscores.da.10)))
		#setnames(xcorpus.pscores.da.10, c("niteid.10"), c("niteid"))
		xcorpus.ovl.da <- read.lex.pros(prosdir=dadir, pattern=paste(prefix, ".*ovl.da.txt", sep=""))
		setkey(xcorpus.ovl.da, niteid)

		xcorpus.pscores.ovl.da <- xcorpus.ovl.da[xcorpus.pscores.da.10]
	#	xcorpus.pscores.ovl.da <- xcorpus.ovl.da[xcorpus.pscores.da]

		setkey(xcorpus.pscores.ovl.da, niteid)
		setkey(xcorpus.tf.pros.da, niteid)
		xcorpus.x.tf.pros.da <- xcorpus.pscores.ovl.da[xcorpus.tf.pros.da] 
	}	
	else {
		xcorpus.x.tf.pros.da <- xcorpus.tf.pros.da

	}

	print("join in DA prosody")
	print(names(xcorpus.x.tf.pros.da))
	## Join in DA prosody 	
	setkey(xcorpus.x.tf.pros.da, niteid)
	setkey(xcorpus.if, niteid)
	xcorpus.x.tf.pros.da <- xcorpus.if[xcorpus.x.tf.pros.da]
	setkey(xcorpus.x.tf.pros.da, niteid)

	#----------------------------------------------------------------	
	# The EDA bit
	#----------------------------------------------------------------	
	if (manual.eda==T) {
		print("## get all edas")
		## get all edas
		xcorpus.eda.da <- add.eda.info.da(xcorpus.x.tf.pros.da, corpus=corpus, 
				eda.file=paste(datadir, "/", corpus, ".ext.txt", sep=""))
		setkey(xcorpus.eda.da, niteid)	
		setnames(xcorpus.eda.da, c("annot"), c("ext.annot"))

		print("## Get linked EDAs")
		## Get linked EDAs
		abs.names <- c("abs.id", "s.id", "slink.id", "niteid", "starttime","endtime","da.type")
		xcorpus.abs <- get.abs.ext.dt(filename=paste(datadir, "/", corpus, ".abs.da.txt", sep=""), corpus="ami", abs.names=abs.names)
		xcorpus.eda.da <- data.table(link.eda=xcorpus.eda.da[, niteid %in% xcorpus.abs$niteid], xcorpus.eda.da)		

		## Add the gold standard annotator for the conversation
		conv.annot <- xcorpus.abs[, list(n.links=length(slink.id)),by=list(annot, conv)]
		setkey(conv.annot, conv)
		setkey(xcorpus.eda.da, conv)
		xcorpus.eda.da <-  conv.annot[xcorpus.eda.da]

		xcorpus.eda.da <- data.table(xcorpus.eda.da, eda.time=xcorpus.eda.da$dur)
		xcorpus.eda.da$eda.time[xcorpus.eda.da$link.eda == F] <- 0
		group.fx0 <- xcorpus.eda.da[!is.na(annot)] #[!is.na(mean.overall)]  #[!is.na(mpos)] #[!(niteid %in% nowords$da.id)]

	 } else {
		group.fx0 <- xcorpus.x.tf.pros.da 	
	 }

        group.fx0 <- fix.group.fx0(group.fx0)

	if (corpus %in% c("ami", "icsi")) {
		print("## get test.convs")
		## get test.convs 
		test.convs <- c(get.ami.test.convs(), get.icsi.test.convs())
		dev.convs <- c(get.ami.dev.convs(), get.icsi.dev.convs()) 
		train.convs <- group.fx0[!(conv %in% test.convs)][!(conv %in% dev.convs), unique(conv)]
		align.errors <- group.fx0[, niteid[dur > 30 & (conv %in% train.convs)]]
		group.fx0 <- group.fx0[!(niteid %in% align.errors)]
	}

	print(outfile)
        save(group.fx0, file=outfile)
	#write.table(group.fx0[1:100], file=paste(outfile, ".txt", sep=""))	
	print("return group.fx0")
	return(group.fx0)

}

## Set NA values to minimum values for that feature.
fix.group.fx0 <- function(group.fx0, target=4) {

        if (sum(is.na(group.fx0$mean.normF0.1)) > 0) {
                f0feats <- grep("F0", names(group.fx0), value=T)
                for (f0feat in f0feats) {
                        group.fx0[[f0feat]][is.na(group.fx0[[f0feat]])] <- min(group.fx0[[f0feat]],na.rm=T)

                }
        }

        if (sum(is.na(group.fx0$mean.normI0.1)) > 0) {
                i0feats <- grep("I0", names(group.fx0), value=T)
                for (i0feat in i0feats) {
                        group.fx0[[i0feat]][is.na(group.fx0[[i0feat]])] <- min(group.fx0[[i0feat]],na.rm=T)
                }
        }

	if (length(grep("overall", names(group.fx0))) > 0)  {
		if (sum(is.na(group.fx0$sd.overall)) > 0) {
			xfeats <- grep("overall", names(group.fx0), value=T)
			for (xfeat in xfeats) {
				print(xfeat)
				group.fx0[[xfeat]][is.na(group.fx0[[xfeat]])] <- min(group.fx0[[xfeat]],na.rm=T)
				print(summary(group.fx0[[xfeat]]))

			}
		}

		if (sum(is.na(group.fx0$sd.horizontal)) > 0) {
			xfeats <- grep("horizontal", names(group.fx0), value=T)
			for (xfeat in xfeats) {
				group.fx0[[xfeat]][is.na(group.fx0[[xfeat]])] <- min(group.fx0[[xfeat]],na.rm=T)
			}
		}

		if (sum(is.na(group.fx0$sd.vertical)) > 0) {
			xfeats <- grep("vertical", names(group.fx0), value=T)
			for (xfeat in xfeats) {
				group.fx0[[xfeat]][is.na(group.fx0[[xfeat]])] <- min(group.fx0[[xfeat]],na.rm=T)
			}
		}
	}
        return(group.fx0)
}

########################################################################
## Main  
########################################################################

args=(commandArgs(TRUE))


if(length(args)==0){
        print("No arguments supplied.")
} else {
        print(args)
	print("************")	
        fsetname <- args[1]
        corpus <- args[2]
        datadir <- args[3]
	pscores <- as.logical(args[4])
	lextype <- args[5]
	prefix <- args[6]
	wtype <- args[7]

	print("Proc feats")
	print(prefix)

	if (fsetname == "aug.wsw") {
		#fsets <- c("tf-if-slope-0.0.all_9-72", "tfsp-if-slope-0.0.all_12-96","if-0.0.all_8-72")
		fsets <- c("tf-if-slope-0.0.all_9-72")
	} else if (fsetname == "aug.sw") {
		fsets <- c("tfsp-if-sw-slope-0.0.all_12-120","if-sw-0.0.all_8-80",
			"tf-if-sw-slope-0.0.all_9-90", "tfsp-sw-0.0.all_4-16")
	} else if (fsetname == "aug.sw.icsi"){
		fsets <- c("tf-da", "tf-if-sw-slope-0.0.all_9-63",
			"tf-if-sw-2.2.all_90-450", "tfsp-if-sw-slope-0.0.all_12-96",
			"tfsp-sw-0.0.all_4-20", "if-sw-0.0.all_8-64", "pmi-if-sw-0.0.all_10-50")

	} else {
		print(paste("unknown feature set", fsetname)) 
		return(1)
	}

	outfile <- paste(datadir, "/da-feats/", prefix, ".", corpus, ".group.fx0.", fsetname, ".", wtype, sep="")
	print(outfile)

	if (corpus %in% c("ami", "icsi")) {
		manual.eda <- T
		lexsuffix <- ".da.spk.txt"
	} else if (corpus == "ted"){
		manual.eda <- F
		lexsuffix <- ".utt.spk.txt"
	} else {
		manual.eda <- F
		#lexsuffix <- ".asrutt.spk.txt"
		lexsuffix <- paste(".", wtype, ".spk.txt", sep="")	
	}
	print(c(corpus, manual.eda, lexsuffix))
	get.feats.main(corpus=corpus, datadir=datadir, lexsets=fsets, pscores=pscores, head=F, outfile=outfile, 
		manual.eda=manual.eda, lextype=lextype, lexsuffix=lexsuffix, prefix=prefix, wtype=wtype)
}
print("END")
