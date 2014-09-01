library(C50)
library(data.table)

################################################
args=(commandArgs(TRUE))
print(args)
if(length(args)==0){
        stop("No arguments supplied. Exiting.")
}
################################################

treefile<- args[1]
testfile<- args[2]

x <- load("treefile")
punctree <- get(x)
test.pos <- data.table(read.csv(testfile, header=F))

punctreePred <- predict(punctree, test.pos)
punctreeProbs <- data.table(test.pos, predict(punctree, test.pos, type ="prob"))

save(punctreeProbs, file="punctreeProbs")



