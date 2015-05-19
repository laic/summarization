library(data.table)
library(ggplot2)
plot.btest.qs <- function() {
	btest <- data.table(read.csv("~/mturk/surveymonkey/btest/CSV/Sheet_1.csv"))
	setnames(btest,  names(btest)[10:18], paste("q",1:9,sep="" ))

	btest <- btest[q9 != ""]
	btest <- btest[, list(q1,q2,q3,q4,q5,q6,q7,q8,q9)] 
	levels(btest$q7)[grep("staying", levels(btest$q7))] <- "She was asking lmaiuton how long\n she was staying at the meeting"
	levels(btest$q7)[grep("Herve", levels(btest$q7))] <- "A comment about\n something Herve Bourlard said"

	for (q in c("q1","q2","q3","q4","q5","q6","q7","q8")) {
		setnames(btest, q, "val")	
		btest.val <- btest[,length(q9)/nrow(btest.val),by=val]
		if (q == "q7") {
			btest.val <- rbindlist(list(btest.val, data.table(q1="She wanted Marc to adjust his video", V1=0)))
		}

		p <- ggplot(btest.val, aes(x=val, y=V1, fill=val)) + geom_bar(stat="identity") 
		p <- p + ylab("%") + xlab("") + ggtitle(q) + theme_bw()
		p <- p + coord_flip() + theme(legend.position = "none") 
		print(p)
		outfile <- paste("~/inevent-svn/Deliverables/D4.6/Images/btest-", q, ".png", sep="")
		ggsave(outfile)
		setnames(btest, "val", q)	

	}

}


proc.survey1 <- function(plot.pics=F) {

sus.qs <- c("I think that I would like to use the video browser frequently.",
	"I found the system unnecessarily complex.",
	"I thought the system was easy to use.",
	"I think that I would need the support of a technical\nperson to be able to use this system.",
	"I found the various functions in this system \nwere well integrated.",
	"I thought there was too much inconsistency in this system.",
	"I would imagine that most people would learn \nto use this system very quickly.",
	"I found the system very cumbersome to use.",
	"I felt very confident using the system.",
	"I needed to learn a lot of things before \nI could get going with this system"
)	
names(sus.qs) <- paste("sus", 1:10, sep="") 

s1 <- data.table(read.delim("~/mturk/surveymonkey/survey1/CSV/Sheet_2.csv", header=T,skip=1))


setnames(s1,  names(s1)[10:19], paste("sus",1:10,sep="" ))
setnames(s1,  names(s1)[20:27], paste(c("hopper","graph","graph.zoom","text.chat","ASR","ASR+slide","audience.reaction","comments"), "useful",sep="."))
setnames(s1,  names(s1)[28:35], paste(c("hopper","graph","graph.zoom","text.chat","ASR","ASR+slide","audience.reaction","comments"), "novel",sep="."))
setnames(s1,  names(s1)[36], "price")
setnames(s1,  names(s1)[37:92], paste(names(s1)[37:92], "graph", sep="."))
setnames(s1,  names(s1)[93:148], gsub(".1$", ".vidpage", names(s1)[93:148] ))
setnames(s1,  names(s1)[149], "q9")

s1 <- s1[q9 != ""]

if (plot.pics) {
	for (q in c("sus1","sus2","sus3","sus4","sus5","sus6","sus7","sus8", "sus9","sus10")) {
		setnames(s1, q, "val")	
		s1.val <- s1[,length(q9)/nrow(s1),by=val]
		s1.val$val <- factor(unlevel(s1.val$val), levels=c("Strongly Disagree","Disagree","Neither Disagree Nor Agree","Agree", "Strongly Agree"))

		s1$val <- factor(unlevel(s1$val), levels=c("Strongly Disagree","Disagree","Neither Disagree Nor Agree","Agree", "Strongly Agree"))
		p <- ggplot(s1.val, aes(x=val, y=V1, fill=val)) + geom_bar(stat="identity") 
		p <- p + ylab("%") + xlab("") + ggtitle(sus.qs[q]) + theme_bw() + coord_flip()
		p <- p + theme(legend.position = "none") 
		print(p)
		outfile <- paste("~/inevent-svn/Deliverables/D4.6/Images/s1-", q, ".png", sep="")
		ggsave(outfile)
		setnames(s1, "val", q)	


	}
}

return(s1)

}

proc.s1.main <- function() {
	s1.melt <- rbindlist(list(data.table(melt(data.frame(s1[,list(q9, 
		sus1=as.numeric(sus1),sus3=as.numeric(sus3),
		sus5=as.numeric(sus5),sus7=as.numeric(sus7),
		sus9=as.numeric(sus9)
		)]))), 
	 	data.table(melt(data.frame(s1[,list(q9, 
		sus2=as.numeric(sus2),sus4=as.numeric(sus4),
		sus6=as.numeric(sus6),sus8=as.numeric(sus8),
		sus10=as.numeric(sus10)
		)])))))
	
	p <- ggplot(s1.melt, aes(x=value)) + geom_histogram() + facet_wrap(~variable) 	
	p <- p + ggtitle("Standard Usability Survey Results")
	print(p)
	ggsave("~/inevent-svn/Deliverables/D4.6/Images/s1-sus-all.png")
	
	s1.melt <- rbindlist(list(data.table(melt(data.frame(s1[,list(q9, 
		sus1=as.numeric(sus1)-1,sus3=as.numeric(sus3)-1,
		sus5=as.numeric(sus5)-1,sus7=as.numeric(sus7)-1,
		sus9=as.numeric(sus9)-1
		)]))), 
	 	data.table(melt(data.frame(s1[,list(q9, 
		sus2=5-as.numeric(sus2),sus4=5-as.numeric(sus4),
		sus6=5-as.numeric(sus6),sus8=5-as.numeric(sus8),
		sus10=5-as.numeric(sus10)
		)])))))

	s1.useful <- s1[,c(grep(".useful", names(s1), value=T), "q9"), with=F]
	for (q in grep("useful", names(s1.useful), value=T)) {
		print(q)	
		s1.useful[[q]] <- as.numeric(gsub("[^0-9]", "",  s1.useful[[q]]))
	}	

	useful.melt <- data.table(melt(data.frame(s1.useful)))
	useful.bin <- useful.melt[,length(q9)/nrow(s1.useful),by=list(variable, value)]
	useful.bin$variable <- gsub(".useful", "", useful.bin$variable)
	p <- ggplot(useful.bin, aes(x=value, y=V1, fill=factor(value))) + geom_bar(stat="identity") + facet_wrap(~variable)
	p <- p + ggtitle("Perceived usefulness of portal features") + theme_bw()
	p <- p + theme(legend.position = "none") + ylab("%") + xlab("Rating (1-7)") 
	print(p)
	ggsave("~/inevent-svn/Deliverables/D4.6/Images/s1-useful.png")

	useful.mean <- useful.melt[,list(mean=mean(value), se=sd(value)/sqrt(length(value))),by=variable]
	useful.mean$variable <- gsub(".useful", "", useful.mean$variable)
	p <- ggplot(useful.mean, aes(x=variable, y=mean, ymax=mean+se, ymin=mean-se, fill=factor(variable))) + geom_bar(stat="identity") + geom_errorbar(width=0.25) + theme_bw()
	p <- p + theme(legend.position = "none") + xlab("") + ylab("Mean rating (1-7)")
	p <- p + scale_y_continuous(limits=c(0,7)) + theme(axis.text.x = element_text(angle=60, hjust=1, size=12))
	p <- p + ggtitle("Mean component usefulness ratings") 
	print(p)
	ggsave("~/inevent-svn/Deliverables/D4.6/Images/s1-useful-mean.png")

	s1.novel <- s1[,c(grep(".novel", names(s1), value=T), "q9"), with=F]
	for (q in grep("novel", names(s1.novel), value=T)) {
		print(q)	
		s1.novel[[q]] <- as.numeric(gsub("[^0-9]", "",  s1.novel[[q]]))
	}	

	novel.melt <- data.table(melt(data.frame(s1.novel)))
	novel.bin <- novel.melt[,length(q9)/nrow(s1.novel),by=list(variable, value)]
	novel.bin$variable <- gsub(".novel", "", novel.bin$variable)
	p <- ggplot(novel.bin, aes(x=value, y=V1, fill=factor(value))) + geom_bar(stat="identity") + facet_wrap(~variable)
	p <- p + ggtitle("Perceived novelty of portal features") + theme_bw()
	p <- p + theme(legend.position = "none") + ylab("%") + xlab("Rating (1-7)") 
	print(p)
	ggsave("~/inevent-svn/Deliverables/D4.6/Images/s1-novel.png")
	
 	novel.mean <- novel.melt[,list(mean=mean(value), se=sd(value)/sqrt(length(value))),by=variable]
	novel.mean$variable <- gsub(".novel", "", novel.mean$variable)
	p <- ggplot(novel.mean, aes(x=variable, y=mean, ymax=mean+se, ymin=mean-se, fill=factor(variable))) + geom_bar(stat="identity") + geom_errorbar(width=0.25) + theme_bw()
	p <- p + theme(legend.position = "none") + xlab("") + ylab("Mean rating (1-7)")
	p <- p + scale_y_continuous(limits=c(0,7)) + theme(axis.text.x = element_text(angle=60, hjust=1, size=12))
	p <- p + ggtitle("Mean component novelty ratings") 
	print(p)
	ggsave("~/inevent-svn/Deliverables/D4.6/Images/s1-novel-mean.png")

	un.mean <- rbindlist(list(data.table(novel.mean, x="novel"), data.table(useful.mean, x="useful")))
	v <- data.table(melt(data.frame(un.mean)))
	setnames(v, c("component","aspect","var","val"))
	
#	p <- ggplot(v[var=="mean"], aes(x=component, y=val,  group=aspect, fill=aspect)) + geom_bar(stat="identity", position="dodge") + theme_bw()
	p <- ggplot(v[var=="mean"], aes(x=component, y=val, fill=aspect)) + geom_bar(stat="identity", position="dodge") + facet_wrap(~aspect) + theme_bw()
	p <- p + scale_y_continuous(limits=c(0,7))  + theme(axis.text.x = element_text(angle=20, hjust=1))
	p <- p + xlab("")
	p <- p + ggtitle("Mean component usefulness and novelty ratings") 
	print(p)
	ggsave("~/inevent-svn/Deliverables/D4.6/Images/s1-novel-useful-mean.png")

	## Phrase list 
	s1.graph <- s1[,c(grep("[a-z].graph", names(s1), value=T), "q9"), with=F]
	s1.graph$Patronizing.graph <- "" ## No patronizing, just a type error
	graph.melt <- data.table(melt(data.frame(s1.graph), id.vars=c("q9")))
	xtable(graph.melt[,length(q9),by=list(variable, value)][value!=""][order(V1,decreasing=T)][1:20, list(value, round(V1/nrow(s1),2))])
	graph20 <- graph.melt[,length(q9),by=list(variable, value)][value!=""][order(V1,decreasing=T)][1:20, list(value, round(V1/nrow(s1),2))]

	s1.vidpage <- s1[,c(grep("[a-z].vidpage", names(s1), value=T), "q9"), with=F]
	vidpage.melt <- data.table(melt(data.frame(s1.vidpage), id.vars=c("q9")))
	xtable(vidpage.melt[,length(q9),by=list(variable, value)][value!=""][order(V1,decreasing=T)][1:20, list(value, round(V1/nrow(s1),2))])
	vid20 <- vidpage.melt[,length(q9),by=list(variable, value)][value!=""][order(V1,decreasing=T)][1:20, list(value, round(V1/nrow(s1),2))]	
	
	graph.all <- graph.melt[,length(q9),by=list(variable, value)][value!=""][order(V1,decreasing=T)][, list(value, round(V1/nrow(s1),2))]
	vid.all <- vidpage.melt[,length(q9),by=list(variable, value)][value!=""][order(V1,decreasing=T)][, list(value, round(V1/nrow(s1),2))]	
}


proc.s2 <- function() {
	s2 <- data.table(read.delim("~/mturk/surveymonkey/survey2/CSV/Sheet_2.csv", header=T,skip=1))

	s2$slide <- factor(s2$slidetrans)
	levels(s2$slide) <- c("line-by-line", "grouped by slide")
	p <- ggplot(s2[,length(slidetrans)/nrow(s2),by=slide], aes(x=slide, y=V1, fill=slide)) + geom_bar(stat="identity") + theme_bw()
	p <- p + theme(legend.position = "none") + xlab("") + ylab("% participants")
	p <- p + ggtitle("Transcript segmentation")  + coord_flip()
	print(p)
	ggsave("~/inevent-svn/Deliverables/D4.6/Images/s2-slide-trans.png")

	s2$dot <- factor(s2$dottrans)
	levels(s2$dot) <- c("word conf. > 0.7", "all words")
	p <- ggplot(s2[,length(dottrans)/nrow(s2),by=dot], aes(x=dot, y=V1, fill=dot)) + geom_bar(stat="identity") + theme_bw()
	p <- p + theme(legend.position = "none") + xlab("") + ylab("% participants")
	p <- p + ggtitle("Words shown in transcript") + coord_flip()
	print(p)
	ggsave("~/inevent-svn/Deliverables/D4.6/Images/s2-dot-trans.png")

	s2$quotetag <- factor(s2$quote)
	levels(s2$quotetag) <- c("quotes", "tagcloud", "tagcloud + quote")
	p <- ggplot(s2[,length(quote)/nrow(s2),by=quotetag], aes(x=quotetag, y=V1, fill=quotetag)) + geom_bar(stat="identity") + theme_bw()
	p <- p + theme(legend.position = "none") + xlab("") + ylab("% participants")
	p <- p + ggtitle("Video summary type")  + coord_flip()
	print(p)
	ggsave("~/inevent-svn/Deliverables/D4.6/Images/s2-quotetag.png")

}


proc.browser.test <- function() {
btest <- data.table(read.csv("~/mturk/surveymonkey/btest/CSV/Sheet_1.csv"))
setnames(btest,  names(btest)[10:18], paste("q",1:9,sep="" ))

btest <- btest[q9 != ""]
btest <- btest[, list(q1,q2,q3,q4,q5,q6,q7,q8,q9)] 



for (q in c("q1","q2","q3","q4","q5","q6","q7","q8")) {
	setnames(btest, q, "val")	
	btest.val <- btest[,length(q9),by=val]
	if (q == "q7") {
		btest.val <- rbindlist(list(btest.val, data.table(q1="She wanted Marc to adjust his video", V1=0)))
	}

	p <- ggplot(btest.val, aes(x=val, y=V1, fill=val)) + geom_bar(stat="identity") 
	p <- p + ylab("Count") + xlab("") + ggtitle(q) + theme_bw()
	p <- p + coord_flip() + theme(legend.position = "none") 
	outfile <- paste("~/inevent-svn/Deliverables/D4.6/Images/btest-", q, ".png", sep="")
	ggsave(outfile)
	setnames(btest, "val", q)	

}

passed.test <-  btest[q1=="a video recording"][q2=="The videos have similar content"][q3=="Those videos aren't inspiring"]
passed.test <- passed.test[q4=="They are the current search results"][q5=="They are words spoken often in this video"]
passed.test <- passed.test[q6=="None"][q7=="A slide she'd added to a presentation"][q8=="Andrei Popescu-Belis"]


btest.ans <- btest[,list(a1=(q1=="a video recording"), 
	a2=(q2=="The videos have similar content"),
	a3=(q3=="Those videos aren't inspiring"),
	a4=(q4=="They are the current search results"),
	a5=(q5=="They are words spoken often in this video"),
	a6=(q6=="None"),
	a7=(q7=="A slide she'd added to a presentation"),
	a8=(q8=="Andrei Popescu-Belis")
), by=q9]


btest.ans.long <- data.table(melt(data.frame(btest.ans[q9!=""]), id.vars="q9"))
btest.scores <- btest.ans.long[,list(scores=sum(value)),by=q9]
ggplot(btest.scores, aes(x=scores)) + geom_histogram() + theme_bw() + ggtitle("MTurk Test Scores") + ylab("No. participants")
ggsave("~/inevent-svn/Deliverables/D4.6/Images/mturk-btest-scores.png")




}
