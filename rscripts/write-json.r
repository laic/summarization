library(jsonlite)

summary.probs.to.json <- function(x) {
	print(x)
	setnames(x, c("trans"), c("text")) 
	v <- x[,list(classId="transcript", speakerId=unlist(lapply(strsplit(unlevel(niteid), split="\\."), function(x) {x[2]})), 
		startTime=starttime, endTime=endtime, SummaryProb=logit.val, trans=text)]
	v <- v[order(startTime)]
	speakers<- unique(v$speakerId)
	v.json <- toJSON(list(classes=c("transcript"), speakers=speakers, segments=v), pretty=T)
	return(v.json)
}
