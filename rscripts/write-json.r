library(jsonlite)
library(data.table)
source("../rscripts/basics.r")

summary.probs.to.json.top10 <- function(x0) {
	print(x0)
	x <- copy(x0) 
	x <- x[order(logit.val, decreasing=T)][1:10]

	if ("trans" %in% names(x)) { 
		setnames(x, c("trans"), c("text")) 
	}
	v <- x[,list(classId="transcript", speakerId=unlist(lapply(strsplit(unlevel(niteid), split="\\."), function(x) {x[2]})), 
		startTime=starttime, endTime=endtime, SummaryProb=logit.val, trans=text)]
	v <- v[order(startTime)]
	speakers<- unique(v$speakerId)
	v.json <- toJSON(list(classes=c("transcript"), speakers=speakers, segments=v), pretty=T)
	return(v.json)
}

summary.probs.to.json <- function(x) {
	print(x)
	if ("trans" %in% names(x)) { 
		setnames(x, c("trans"), c("text")) 
	}
	v <- x[,list(classId="transcript", speakerId=unlist(lapply(strsplit(unlevel(niteid), split="\\."), function(x) {x[2]})), 
		startTime=starttime, endTime=endtime, SummaryProb=logit.val, trans=text)]
	v <- v[order(startTime)]
	speakers<- unique(v$speakerId)
	v.json <- toJSON(list(classes=c("transcript"), speakers=speakers, segments=v), pretty=T)
	return(v.json)
}

words.dt.to.json <- function(x) {
	v <- x[ , {
		z <- data.table(wordStart=wstart, wordEnd=wend, wordId=word, wordConfidence)
		list(classId=unique(classId), startTime=min(wstart), endTime=max(wend), 
			speakerId=unique(unlist(lapply(strsplit(unlevel(niteid), split="\\."), function(x) {x[2]}))),
			spokenWords=list(z))
		}, by=sent.id]

	speakers<- unique(v$speakerId)
	classes <- unique(v$classId)
	v$sent.id <- NULL
	v <- v[order(startTime)]
	v.json <- toJSON(list(classes=classes, speakers=speakers, segments=v), pretty=T)
	return(v.json)
}

