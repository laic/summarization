source("../sentence_breaks/punc-classifier.r")

################################################
args=(commandArgs(TRUE))
print(args)
if(length(args)==0){
        stop("No arguments supplied. Exiting.")
}
################################################

posdir <- args[1]

get.punctree(posdir, treefile="./punctree")

