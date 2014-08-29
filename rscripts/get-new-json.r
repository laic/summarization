## Text processing for a new conv
source("../rscripts/f0basics.r")
source("../rscripts/read-json.r")
source("../rscripts/nxt-proc.r")
#source("~/scripts/proc-lex.r")
library(data.table)
library(plyr)
library(tm)

get.lex.from.json <- function(filename, corpus="inevent", 
	vidsrc=tolower(gsub("[^A-Z]", "", basename(filename))), 
	filelist="~/inevent-svn/WPs/WP2/UEDIN_ASR_201407/filenames.txt", 
	datadir=paste("~/data/", corpus, "/derived/", sep="") 
)
{
        ## Filename data
        info.dt <- data.table(read.delim(filelist, header=T))

        ## Get segments and words from json file
	print("=== read.json ===")
        words.dt <- read.json(filename=filename, info.dt=info.dt)

	## Write out utterance times in flat format
	print("=== write utts times ===")
	uttdir <- paste(datadir, "/asrutt/", sep="")
        if (!file.exists(uttdir)) {
                dir.create(uttdir, recursive=T)
                print(uttdir)
        }

        utts <- unique(words.dt[,list(conv, longconv, wav.file, video.file, niteid=seg.id, spk=speakerId, starttime=startTime, endtime=endTime)])
	if (is.factor(utts$conv)) { utts$conv <- unlevel(utts$conv)}
        #utts <- data.table(utts, participant=utts[,unlist(lapply(strsplit(longconv, split="_"), function(x) {x[1]}))])
        utts <- data.table(utts, participant=utts$spk)
	utts <- utts[order(starttime)]
        write.conv.seg(utts, dirname=uttdir, segname="asrutt")

	## Just use diarization output for spurts
	print("=== write spurts for prosody processing ===")
	spurts <- utts[,{
                sid <- unlist(lapply(strsplit(niteid, split="\\."), function(x) {x[4]}))
		list(conv=conv, spk=spk, participant=participant, sid=sid, chno=NA, vidsrc=vidsrc,
                starttime=starttime, endtime=endtime, niteid=niteid, 
		longconv=longconv, wav.file=wav.file, video.file=video.file 
                )}]

#	if (is.factor(spurts$conv)) { spurts$conv <- unlevel(spurts$conv)}
	write.features.by.conv(spurts, dirname=uttdir, fsuffix=".asrspurts", plain.txt=T)

#        ddply(spurts, .(conv), function(x) {
#                write.table(x, file=paste(uttdir, unique(x$conv), ".asrspurts.txt", sep=""), row.names=F, col.names=F, quote=F)
#                })


	print("=== write word times ===")
        ## Put words.dt in the format expected to calculate tf.idf etc
	worddir <- paste(datadir, "/asrword/", sep="")
        if (!file.exists(worddir)) {
                dir.create(worddir, recursive=T)
                print(worddir)
        }


	setnames(words.dt, c("word.id","startTime","endTime","speakerId","wordStart","wordEnd","wordId"),
		c("niteid","starttime","endtime","spk","wstart","wend","word"))
	write.features.by.conv(words.dt, dirname=worddir, fsuffix=".raw.asrword", plain.txt=F)
	
        xwords.dt0 <- words.dt[,list(niteid, conv, spk, starttime=wstart, endtime=wend)]
        write.conv.seg(xwords.dt0, dirname=worddir, segname="asrword")
}

########################################################################

args=(commandArgs(TRUE))
print(args)
if(length(args)==0){
	stop("No arguments supplied. Exiting.")
} 

filename <- args[1]
corpus <- args[2]
info.file <- args[3]

datadir <- Sys.getenv("DATADIR") 
if (datadir=="") {
	print("No DATADIR in environment. Set to default.")
	datadir <- paste("~/data/", corpus, "/derived/", sep="")
	print(paste("datadir:", datadir)) 
} 

if (!file.exists(datadir)) {
	dir.create(datadir, recursive=T)
	print(paste("dir.create datadir:", datadir))
}

vidsrc <- tolower(gsub("[^A-Z]", "",  basename(filename)))
get.lex.from.json(filename, corpus="inevent", vidsrc=vidsrc, 
	filelist=info.file, 
	datadir=datadir)  

print("=== END get-new-json ===")
