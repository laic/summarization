library(data.table)
library(ggplot2)



get.qrank <- function(x) {
	x.id <- unique(rbindlist(list(x[,list(conv=Input.conv, niteid=Input.firstid, lval=Input.firstlval)], 
			x[,list(conv=Input.conv, niteid=Input.secondid, lval=Input.secondlval)])))

	x.id <- x.id[order(conv, lval)]
	qrank <- x.id[,list(niteid, lval, qrank=order(lval,decreasing=T)),by=conv]

	return(qrank)

}

main <- function() {
x <- data.table(read.csv("~/mturk/quoteresults.csv", header=T))

qrank <- get.qrank(x)

y <- x[,list(hit=HITId, conv=Input.conv, q1=Input.firstid, q2=Input.secondid, Ans=Answer.Q1Answer, 
	firsthigh=Input.firstlval > Input.secondlval, 
	nwords1=unlist(lapply(strsplit(unlevel(Input.firstquote), split=" "), length)), 
	nwords2=unlist(lapply(strsplit(unlevel(Input.secondquote), split=" "), length)), 
	trans1=Input.firstquote, trans2=Input.secondquote)]

setnames(y, "q1", "niteid")
setkey(y, niteid, conv)
setkey(qrank, niteid, conv)
y <- qrank[y]
setnames(y, "niteid", "q1")
setnames(y, c("lval", "qrank"), c("lval1","qrank1"))

setnames(y, "q2", "niteid")
setkey(y, niteid, conv)
y <- qrank[y]
setnames(y, "niteid", "q2")
setnames(y, c("lval", "qrank"), c("lval2","qrank2"))


r1 <- y$qrank1
r2 <- y$qrank2
first.ans <- (y$Ans == 1)

r1[y$qrank1 > y$qrank2] <- y$qrank2[y$qrank1 > y$qrank2]
r2[y$qrank1 > y$qrank2] <- y$qrank1[y$qrank1 > y$qrank2]
first.ans[y$qrank1 > y$qrank2] <- !first.ans[y$qrank1 > y$qrank2]
y <- data.table(y, r1=r1, r2=r2, first=first.ans, nwordsdiff=y$nwords1-y$nwords2)

v <- y[, list(conv, select1=length(Ans[Ans==1]), lvaldiff=(lval1-lval2)),by=list(hit, q1, q2, lval1, lval2, r1, r2, qrank1, qrank2, nwordsdiff, firsthigh, trans1, trans2)]

v <- y[, list(select1=length(Ans[Ans==1]), lvaldiff=(lval1-lval2)),by=list(hit, q1, q2, lval1, lval2, r1, r2, qrank1, qrank2, nwordsdiff, firsthigh)]

p <- ggplot(v, aes(x=abs(lvaldiff), y=select.top)) + geom_jitter()
p + xlab("Abs. difference in probabilities") + ylab("# higher rank selected")
p + xlab("Abs. difference in probabilities") + ylab("# higher rank selected")
ggsave("~/inevent-svn/Deliverables/D2.5update/Images/quote-prob-rank.png")



u <- y[,list((sdiff=length(conv[Ans==1]) - length(conv[Ans==2])), select1=length(conv[Ans==1])),by=list(hit, firsthigh)]
u$highrank <- rep("1", nrow(u))
u$highrank[u$firsthigh==F] <- "2"


p <- ggplot(u, aes(group=firsthigh, x=select1, fill=firsthigh)) + geom_histogram(position="dodge")    
p + xlab("User selected 1") + labs(fill="High prob. 1")
ggsave("~/mturk/fig/high-hist.png")


p <- ggplot(u, aes(group=firsthigh, x=select1, fill=firsthigh)) + geom_histogram(position="dodge") 


ggplot(u, aes(x=firsthigh, y=V1)) + geom_boxplot()

p <- ggplot(u, aes(group=firsthigh, x=V1, fill=firsthigh)) + geom_histogram(position="dodge") 

}
