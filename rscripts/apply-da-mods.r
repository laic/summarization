## Assumes you have some feature files saved as an object ("group.fx0" - probably want to rename!)

library(data.table)
library(plyr)
source("../rscripts/proc-eda-mods.r")

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
	da.word.file <- NULL	
	trans.file <- NULL

        fsetname <- args[1]
	dataset <- args[2]
	corpus <- args[3]
	datadir <- args[4]
	mod.dataset <- args[5]	
	mod.datadir <- args[6]
	mod.corpus <- args[7]
	word.file <- args[8]
	wtype <- args[9]

	if (grepl("trans", word.file)) { trans.file <- word.file }
	if (grepl("word", word.file)) { da.word.file <- word.file }
	print(paste("wtype:", wtype))

	print("=== get feature data.table ===")
	print(paste("dataset:", dataset))	
	print(paste("da.word.file:", da.word.file))	
	print(paste("trans.file", trans.file))	


	group.fx0 <- prep.data(filename=paste(datadir, "/da-feats/", dataset, sep=""), da.word.file=da.word.file, trans.file=trans.file)
	#group.fx0 <- prep.data(filename=paste(datadir, "/da-feats/", dataset, sep=""), da.word.file=NULL)
	#print(group.fx0)

	dset.prefix <- strsplit(dataset, "\\.")[[1]][1] 
	#wtype <- tail(strsplit(dataset, "\\.")[[1]], 1)

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
	#print(names(group.fx0))
	apply.fset.mods(group.fx0, fsetname=fsetname, 
			evaldir=evaldir,
			moddir=moddir,
			sevaldir=sevaldir, 
			corpus=corpus, mod.corpus=mod.corpus, dset=dset.prefix, wtype=wtype) 

}

print("END")
