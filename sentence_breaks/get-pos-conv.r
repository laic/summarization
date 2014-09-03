options( java.parameters = "-Xmx100m" )
source("../sentence_breaks/punc-classifier.r")

################################################
args=(commandArgs(TRUE))
print(args)
if(length(args)==0){
        stop("No arguments supplied. Exiting.")
}
################################################
#transdir <- "~/data/ted/traintrans/"
filename <- args[1]
posdir <- args[2]

print(filename)
fstem <- basename(filename)
print(fstem)
pos.conv <- get.stop.labelled.pos.conv(filename)
save(pos.conv, file=paste(posdir, "/",fstem, ".pos", sep=""))

