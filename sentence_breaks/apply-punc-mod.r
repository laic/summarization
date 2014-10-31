#options( java.parameters = "-Xmx100m" )#
library(methods)
source("../sentence_breaks/punc-classifier.r")

################################################
args=(commandArgs(TRUE))
print(args)
if(length(args)==0){
        stop("No arguments supplied. Exiting.")
}
################################################
#transdir <- "~/data/ted/traintrans/"
#posdir <- args[2]
filename <- args[1]
punctreefile <- args[2]
word.var <- args[3]
start.var <- args[4]
word.id <- args[5]

fstem <- basename(filename)

x <- load(punctreefile)
punctree <- get(x)

get.autopunc.words(filename, punctree, word.var=word.var, start.var=start.var, word.id=word.id) 





