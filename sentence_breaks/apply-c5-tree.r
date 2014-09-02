library(C50)
library(openNLP)
library(data.table)
require(NLP)

apply.punctree <- function(treefile, testfile) {
	x <- load(treefile)
	punctree <- get(x)
	test.pos <- data.table(read.csv(testfile, header=F))

	punctreePred <- predict(punctree, test.pos)
	punctreeProbs <- data.table(test.pos, predict(punctree, test.pos, type ="prob"))
	save(punctreeProbs, file="punctreeProbs")
}

demo.tokenizer <- function() {
	s <- paste(c("Pierre Vinken, 61 years old, will join the board as a ",
	"nonexecutive director Nov. 29.\n", "Mr. Vinken is chairman of Elsevier N.V.,
	", "the Dutch publishing group."), collapse = "")


	s <- as.String(s)
	sent_token_annotator <- Maxent_Sent_Token_Annotator()
	word_token_annotator <- Maxent_Word_Token_Annotator()
	a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))
	pos_tag_annotator <- Maxent_POS_Tag_Annotator()
	a3 <- annotate(s, pos_tag_annotator, a2)
	a3

}

word_space_tokenizer <-function(s) {
	s <- as.String(s)
	## Remove the last character (should be a period when using
	## sentences determined with the trivial sentence tokenizer).
	## Split on whitespace separators.
	m <- gregexpr("[^[:space:]]+", s)[[1L]]
	Span(m, m + attr(m, "match.length") - 1L)
}


tokenize.words <- function(currwords) {
	s <- paste(tolower(currwords), collapse= " ")
	s <- as.String(s)
	sent_token_annotator <- Maxent_Sent_Token_Annotator()
	word_token_annotator <- Maxent_Word_Token_Annotator()
	tok.words <- annotate(s, list(sent_token_annotator, word_token_annotator))
	pos_tag_annotator <- Maxent_POS_Tag_Annotator()
	pos.words <- annotate(s, pos_tag_annotator, tok.words)
	

	pw <- subset(pos.words, type == "word")
	pw.dt <- data.table(id=pw$id, start=pw$start, end=pw$end, unlist(pw$features))
	pw.dt <- pw.dt[,list(start, end, word=substr(as.character(s), start,end),pos=V4),by=id]

	tags <- sapply(pw$features, `[[`, "POS")
	u <- data.table(s[pw], tags)

	## A simple word token annotator based on the word tokenizer.
	word_token_annotator <- Simple_Word_Token_Annotator(word_space_tokenizer)
	tok.words <- annotate(s, list(sent_token_annotator, word_token_annotator))
	tw <- subset(tok.words, type == "word")
	tw.dt <- data.table(id=tw$id, start=tw$start, end=tw$end)
tw	tw.dt <- tw.dt[,list(start, end, word=substr(as.character(s), start,end)),by=id]

tw.int <- Intervals(tw.dt[,list(start,end)])
pw.int <- Intervals(pw.dt[,list(start,end)])
v <- interval_overlap(tw.int, pw.int)
u <- unlist(lapply(v, function(x) {paste(c(pw.dt$word[x], pw.dt$pos[x]), collapse="")}))



}

################################################
#args=(commandArgs(TRUE))
#print(args)
#if(length(args)==0){
#        stop("No arguments supplied. Exiting.")
#}
################################################

#treefile<- args[1]
#wordfile<- args[1]





