source("../rscripts/read-ted.r")

########################################################################

args=(commandArgs(TRUE))
print(args)
if(length(args)==0){
	stop("No arguments supplied. Exiting.")
} 

filename <- args[1]
transdir <- args[2]
infofile <- args[3]
sentdir <- args[4]

write.ted.plain.txt (filename, 
	transdir=transdir, 
	infofile=infofile,
	sentdir=sentdir)  

print("=== END write-sentences===")
