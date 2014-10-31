## Get ASR output in .json format as specified by the inEvent API
## Assume we have a data.table info.dt which gives us the mapping between
## ASR .json file, video and wav file names
library(jsonlite)

read.json <- 
read.json.asr <- function(filename, info.dt) {
	print(paste("read.json.asr: ", filename))
	x <- fromJSON(filename, validate=T)	
	video.file <- info.dt[json.file == basename(filename)]$video.file	
	wav.file <- info.dt[json.file == basename(filename)]$wav.file	
	#print(wav.file)

	if (is.factor(video.file)) {
		video.file <- unlevel(video.file)
	} 
	if (is.factor(wav.file)) {
		wav.file <- unlevel(wav.file)
		#print(paste("unleveled:", wav.file))
	} 

	## longconv is to account for the fact that the ASR files for TED 
	## had significantly shorter names than associated AV files.
	longconv <- gsub(".wav", "", strsplit(wav.file, split="-")[[1]][1])			
	conv <- gsub(".asr.json", "",  basename(filename))
	conv <- gsub(".json", "",  conv)


	x.segs <- data.table(x$segments)
	x.segs <- x.segs[order(startTime)]
	## get segs
	segs.dt <- data.table(conv=conv, longconv=longconv, wav.file=wav.file, video.file=video.file, 
			x.segs[,list(classId, endTime, startTime, speakerId)])
		#	classId=x$segments$classId, startTime=x$segments$startTime, endTime=x$segments$endTime, speakerId=x$segments$speakerId)
	#segs.dt <- segs.dt[order(startTime)]	
	segs.dt <- data.table(seg.id=paste(conv, segs.dt$speakerId, "seg", 1:nrow(segs.dt), sep="."),  segs.dt)

	#print(segs.dt[1:10])
#	print(segs.dt)

	## get words	
	words.list <- x.segs$spokenWords
	names(words.list) <- segs.dt$seg.id
	words.dt <- data.table(ldply(words.list, function(x){
		y<-NULL
		if (is.data.frame(x)) {
			y <- x
		} 
		return(y)
	}))
	setnames(words.dt, c(".id"), c("seg.id"))	
	#print(words.dt)
	## join back in extra segment information
	setkey(segs.dt, seg.id)
	setkey(words.dt, seg.id)
	words.dt <- segs.dt[words.dt]
	words.dt <- words.dt[order(wordStart)]

	words.dt <- data.table(word.id=words.dt[,paste(conv,".", speakerId, ".words", c(1:nrow(words.dt)), sep="")], words.dt)
	#print(words.dt)
	return(words.dt)
	
}

