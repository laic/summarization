#options( java.parameters = "-Xmx100m" )
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

fstem <- basename(filename)

x <- load(punctreefile)
punctree <- get(x)

get.test.data(filename, punctree, word.var=word.var, start.var=start.var) 



