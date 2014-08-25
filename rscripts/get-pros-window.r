#!/usr/bin/Rscript --vanilla --slave

library(data.table)
library(plyr)
source("../rscripts/proc-prosody.r")

print("=== START get-pros-window ===")

args=(commandArgs(TRUE))
if(length(args)==0){
	stop("No arguments supplied. Exiting")

}

#----------------------------------------------------------
print(args)
currconv <- args[1]
featname <- args[2]
xdir <- args[3]
window.dir  <- args[4]
window.type <- args[5]
#----------------------------------------------------------
## get windows
window.file <- paste(window.dir, "/", currconv, ".conv.", window.type, sep="")		
wobj <- load(window.file)
x.list <- get(wobj)
print(paste("spk NAMES:", names(x.list)))

## get feature time series 

objfile <- paste(xdir, "/", featname, "/", currconv, sep="")		
xobj <- load(objfile)
x.feat <- get(xobj)

## Fix an ICSI error 
if (currconv == "Bmr031") {
	x.feat$participant[x.feat$xid == "Bmr031-H"] <- "me001"
}

## Get aggs
x.aggs <- get.var.aggs.spk(x.feat, windows=x.list, wkey="xid", var.name=toupper(featname))

#if (featname == "i0") {
#	x.aggs <- get.i0.aggs.spk(x.feat, windows=x.list, wkey="xid")
#} else if (featname == "f0") {
#	x.aggs <- get.f0.aggs.spk(x.feat, windows=x.list, wkey="xid")
#}

outfile.txt <- paste(xdir, "/", featname, "/", currconv, ".aggs.", window.type, ".txt", sep="")		
print(outfile.txt)	
write.table(x.aggs, file=outfile.txt, row.names=F, quote=F)	
	
print("=== END get-pros-window ===")

