## Assumes you have some feature files saved as an object ("group.fx0" - probably want to rename!)

library(data.table)
library(plyr)
source("../rscripts/proc-eda-mods.r")

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

###################################################################################################
# START
###################################################################################################

## Read in feature file

args=(commandArgs(TRUE))


if(length(args)==0){
        print("No arguments supplied.")
} else {
	print("=======args==========")
        print(args)
	print("=====================")

        fsetname <- args[1]
	dataset <- args[2]
	corpus <- args[3]
	datadir <- args[4]
	mod.dataset <- args[5]	
	mod.datadir <- args[6]
	mod.corpus <- args[7]
	da.word.file <- args[8]

	print("=== get feature data.table ===")
	print(paste("dataset:", dataset))	
	print(paste("da.word.file:", da.word.file))	
	group.fx0 <- prep.data(filename=paste(datadir, "/da-feats/", dataset, sep=""), da.word.file=da.word.file)
	#group.fx0 <- prep.data(filename=paste(datadir, "/da-feats/", dataset, sep=""), da.word.file=NULL)
	#print(group.fx0)

	dset.prefix <- strsplit(dataset, "\\.")[[1]][1] 

	print("=== make dirs ===")
	print(datadir)
	evaldir <- paste(datadir, "/segs/reval/", mod.dataset, "/", sep="")
	sevaldir <- paste(datadir, "/summeval/", mod.dataset, "/", sep="")
	moddir <- paste(mod.datadir, "/mods/", mod.dataset, "/", sep="")

	if (!file.exists(evaldir)) {
		dir.create(file.path(paste(datadir,"/segs/reval/", sep=""), mod.dataset))
		print(evaldir)
	}	


	if (!file.exists(sevaldir)) {
		dir.create(file.path(paste(datadir,"/summeval/", sep=""), mod.dataset))
		print(sevaldir)
	}	

	print(moddir)

	#da.text <- read.table(paste(da.word.file, ".text", sep=""), header=T)
	print("=== apply models ===")
	print(names(group.fx0))
	apply.fset.mods(group.fx0, fsetname=fsetname, 
			evaldir=evaldir,
			moddir=moddir,
			sevaldir=sevaldir, 
			corpus=corpus, mod.corpus=mod.corpus, dset=dset.prefix) 

}

print("END")
