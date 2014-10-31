#options( java.parameters = "-Xmx100m" )
library(data.table)
library(plyr)
source("../rscripts/basics.r")
source("../rscripts/nxt-proc.r")

write.html.trans <- function(sents.conf, outfile, id.var="niteid") {
	print("HERE")
        write("<html>", file=outfile, append=FALSE)
        write(paste("<head><title>",currconv, threshold, "</title> </head>"), file=outfile, append=T)
        write("<body bgcolor=\"white\">", file=outfile, append=T)

	for (i in 1:nrow(sents.conf)) {
                currda <- sents.conf[[id.var]][i]
                anchor <- paste("<p style=\"white-space:pre-wrap; width:120ex\"><a name=", "\"", currda, "\">", sep="")
                anchor <- paste(anchor, paste("[", currda, "]</a>", sep=""),sep="")
                anchor <- paste(anchor, sents.conf[i]$trans,sep="")
                anchor <- paste(anchor, "</p>",sep="")
                write(anchor, file=outfile, append=T)
        }

	#sents.conf
	#write.table(sents.conf[,list(starttime, endtime, trans)], file=outfile)

	write("</body>", file=outfile, append=T)
        write("</html>", file=outfile, append=T)

}

################################################
args=(commandArgs(TRUE))
print(args)
if(length(args)==0){
        stop("No arguments supplied. Exiting.")
}
################################################
filename <- args[1]
datadir <- args[2]

currsw <- data.table(read.table(filename, header=T))

print("=== write sents times ===")
sentdir <- paste(datadir, "/asrsent/", sep="")
if (!file.exists(sentdir)) {
	dir.create(sentdir, recursive=T)
	print(sentdir)
}

currsw <- unique(currsw[,list(conv, longconv, wav.file, video.file, spk=spk, participant=spk, starttime=sent.start, endtime=sent.end, 
		wstart, wend, word, niteid, wordConfidence, sent.id)])

currsw <- currsw[order(wstart)] 

sents <- unique(currsw[,list(conv, longconv, wav.file, video.file, spk, participant, starttime, endtime, 
		trans=paste(word, collapse=" ")), by=sent.id])


setnames(sents, "sent.id", "niteid")
write.table(sents, file="x.sents.txt")

if (is.factor(sents$conv)) { sents$conv <- unlevel(sents$conv)}

sents <- sents[order(starttime)]
write.conv.seg(sents, dirname=sentdir, segname="asrsent")

currconv <- unique(sents$conv)
trans <- sents[,list(niteid, starttime, endtime, trans=tolower(trans))]

print(paste(sentdir,"/", currconv, ".asrsent.trans.txt", sep=""))
write.table(trans, file=paste(sentdir,"/", currconv, ".asrsent.trans.txt", sep="") )


worddir <- paste(datadir, "/asrword/", sep="")
for (threshold in c(0.0, 0.5, 0.7, 0.9, 1)) {
	currsw.conf <- copy(currsw) 
	currsw.conf <- currsw.conf[order(wstart)]
	currsw.conf$word <- unlevel(currsw.conf$word)

	currsw.conf$word[currsw.conf$wordConfidence < threshold] <- "."

	sents.conf <- unique(currsw.conf[,list(starttime, endtime, 
			trans=paste(tolower(word), collapse=" ")), by=sent.id])
	sents.conf <- sents.conf[order(starttime)]

	outfile <- paste(sentdir,"/", currconv, ".asrsent.",threshold, ".html", sep="")
	print(outfile)

	write.html.trans(sents.conf, outfile, id.var="sent.id")	
#	write.table(currsw.conf[word != "."], file=paste(worddir, "/", currconv, ".autopunc-",threshold,".txt", sep=""))
	write.table(currsw.conf, file=paste(worddir, "/", currconv, ".autopunc-",threshold,".txt", sep=""))

	#newsent <- strsplit(sents.conf$trans, split="\\.")
#
	#currsw.conf <- data.table(read.table("~/data/inevent/derived/asrword/TED0069.autopunc-1.txt", header=T))
	currsw.conf$niteid <- unlevel(currsw.conf$niteid)
	currsw.conf$niteid[currsw.conf$word == "."] <- "@"
	currsw.conf <- currsw.conf[order(wstart)]

	trans <- currsw.conf[,list(trans=paste(niteid, collapse=" ")),by=sent.id]
	trans.split <- trans[,strsplit(trans, split="@"),by=sent.id]
	
	trans.split$newid <- 1:nrow(trans.split)
	trans.split <- trans.split[grep("[a-z0-9]", V1)]
	trans.split.words <- trans.split[,strsplit(V1,split=" "),by=newid]
	trans.split.words <- trans.split.words[grep("[a-z0-9]", V1)]
	setnames(trans.split.words, "V1", "niteid")
	trans.split.words <- trans.split.words[order(newid)]

	setkey(trans.split.words, niteid)
	setkey(currsw.conf, niteid)

	conf.words <- currsw.conf[trans.split.words][order(wstart)]
	conf.words$starttime <- NULL
	conf.words$endtime <- NULL
	conf.words$sent.id <- NULL

	conf.words.times <- conf.words[,list(starttime=min(wstart), endtime=max(wend)),by=newid]
	setkey(conf.words.times, newid)
	setkey(conf.words, newid)

	conf.words <- conf.words.times[conf.words]
	conf.words$sent.id <- conf.words[,paste(conv,".",spk,".autosent",threshold,".",newid, sep="") ]
	write.features.by.conv(conf.words, dirname=sentdir, fsuffix=paste(".raw.autosent",threshold,sep=""))


	conf.words <- conf.words[order(wstart)]	
	sents <- conf.words[,list(trans=paste(tolower(word),collapse=" ")),by=list(sent.id, conv, starttime, endtime, spk, participant)]	
	setnames(sents, "sent.id", "niteid")
	write.conv.seg(sents, dirname=sentdir, segname=paste("autosent",threshold,sep=""))
	write.table(sents[,list(niteid, starttime, endtime, trans)], file=paste(paste(sentdir, "/", currconv,".autosent",threshold,".trans.txt", sep=""))) 

	outfile <- paste(sentdir,"/", currconv, ".autosent.",threshold, ".html", sep="")
	print(outfile)
	write.html.trans(sents, outfile, id.var="niteid")	
}


