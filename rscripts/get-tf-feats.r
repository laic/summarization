## Text processing for a new conv
source("~/scripts/f0basics.r")
source("~/scripts/nxt-proc.r")
source("~/scripts/proc-lex.r")
library(data.table)
library(plyr)
library(tm)

get.tf.feats <- function(currconv, corpus="inevent", 
	datadir=paste("~/data/", corpus, "/derived/", sep=""), 
	idf.conv.file="~/data/misc-derived/all.nxtwords.idf.conv.txt", 
	idf.spk.file="~/data/misc-derived/all.nxtwords.idf.spk.txt", 
        pmi.file="~/data/misc-derived/ami.icsi.eda.pmi.txt"
	word.file.suffix=".raw.asrword.txt"
)
{
        worddir <- paste(datadir, "/asrword/", sep="")
	words.dt <- data.table(read.table(paste(worddir, currconv, word.file.suffix, sep=""), header=T)) 
	
	## Get the words from the current dataset
	## We do it like this basically to be able to correct errors on this dataset
	## We assume that the background idf set won't change      
	print("=== get words from current dataset ===")
	dset.word.files <-as.list(list.files(worddir, pattern=paste(word.file.pattern, "$", sep=""), full.names=T))
	dset.words.dt0 <- rbindlist(lapply(dset.word.files, function(filename) {
		curr.dt <- data.table(read.table(filename,header=T))
		## To match with current format of AMI/ICSI word files
		curr.dt0 <- curr.dt[,list(corpora=corpus,id=word.id, conv=conv, maxtime=max(wordEnd, na.rm=T), spk=speakerId,
                	fname=longconv, .id="w", starttime=wordStart, endtime=wordEnd, word=wordId,
                	clean.word=clean.stem.word(tolower(wordId), stem=T, remove.morph=T))] 	
		return(curr.dt0)
	}))

        xwords.dt0 <- words.dt[,list(corpora=corpus,niteid=word.id, conv=conv, maxtime=max(wordEnd, na.rm=T), spk=speakerId,
                fname=longconv, .id="w", starttime=wordStart, endtime=wordEnd, word=wordId, 
		clean.word=clean.stem.word(tolower(wordId), stem=T, remove.morph=T))]

	## Sanity check
	if (!(unique(xwords.dt0$conv) %in% dset.words.dt0$conv)) {
		print(paste(unique(xwords.dt0$conv), "not in dset.words.dt0:"))
		print(unique(dset.words.dt0$conv))
	}
	print("=== word.idf.conv ===")
	word.idf.conv <- data.table(read.table(file=idf.conv.file, header=T)) 
	word.idf.spk <- data.table(read.table(file=idf.spk.file, header=T)) 

	print("=== update idf table ===")
	print(unique(dset.words.dt0$conv))
	word.idf.conv <- update.idf.conv(dset.words.dt0, word.idf=word.idf.conv, spk=F)
	word.idf.spk <- update.idf.conv(dset.words.dt0, word.idf=word.idf.spk, spk=T)

	print("=== term frequency features ===")
	lexdir <- paste(datadir, "/segs/asrlex/", sep="")
        if (!file.exists(lexdir)) {
		print(paste("create dir: ", lexdir))
                dir.create(lexdir, recursive=T)
        }
	print(paste("lexdir:", lexdir))

	print("=== tf.idf ===")
        x.lex.grp <- collect.tf.feats(xwords.dt0, word.idf.spk=word.idf.spk, word.idf.conv=word.idf.conv)
        write.features.by.conv(x.lex.grp, dirname=lexdir, fsuffix=".lex.grp")

	print("=== pmi ===")
        x.words <- x.lex.grp[,list(conv, spk, niteid, word, clean.word, starttime, endtime)]
        eda.pmi <- data.table(read.table(pmi.file, header=T))
        setkey(eda.pmi, clean.word)
        setkey(x.words, clean.word)
        x.pmi <- eda.pmi[x.words]

        # Fix words that aren't in the training set.
        x.pmi.clean <- clean.pmi(x.pmi)
        print(x.pmi.clean[grep("[^a-z0-9]", clean.word)])

        write.features.by.conv(x.pmi.clean, dirname=lexdir, fsuffix=".pmi")

}

########################################################################

args=(commandArgs(TRUE))
print(args)
if(length(args)==0){
	print("No arguments supplied.")
	print("EXIT")
        return(1)
} 

currconv <- args[1]
corpus <- args[2]
idf.conv.file <- args[3] 
idf.spk.file <- args[4]
pmi.file <- args[5]

datadir <- paste("~/data/", corpus, "/derived/", sep="")
if (!file.exists(datadir)) {
	dir.create(datadir, recursive=T)
	print(paste("datadir:", datadir))
}

get.tf.feats(conv, corpus="inevent", 
	datadir=datadir, 
	idf.conv.file=idf.conv.file, 
	idf.spk.file=idf.spk.file, 
        pmi.file=pmi.file, 
	word.file.suffix=".raw.asrword.txt")

print("=== END: get-tf-feats.r ===")
