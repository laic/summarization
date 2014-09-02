library(C50)
source("../rscripts/")
library(openNLP)

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

tokenize.words <- function(currwords) {
	s <- paste(tolower(currwords), collapse= " ")
	s <- as.String(s)
	sent_token_annotator <- Maxent_Sent_Token_Annotator()
	word_token_annotator <- Maxent_Word_Token_Annotator()
	tok.words <- annotate(s, list(sent_token_annotator, word_token_annotator))
	pos_tag_annotator <- Maxent_POS_Tag_Annotator()
	pos.words <- annotate(s, pos_tag_annotator, tok.words)
	

	pw <- subset(pos.words, type == "word")
	tags <- sapply(pw$features, `[[`, "POS")

	tags
}

################################################
args=(commandArgs(TRUE))
print(args)
if(length(args)==0){
        stop("No arguments supplied. Exiting.")
}
################################################

#treefile<- args[1]
wordfile<- args[1]





