## proc-prosody.r: functions to do with getting aggregates over prosody 
## time series.
## used by: get-pros-window.r
 
source("../rscripts/f0basics.r")
source("../rscripts/read-praat-files.r")
source("../rscripts/nxt-proc.r")

## get aggregates over the window specified in x, to the data in xprops,
## default window size is 15 seconds.

get.var.aggs.spk <- function(xdt, windows=NULL, wkey="xid", wsize=15, var.name="F0") {
	#print(xdt)
	var.aggs.spk <- ddply(xdt, c("conv","nxt_agent"), get.xint.windows,
			windows=windows, wkey=wkey, fx=get.var.aggs.window, wsize=wsize, var.name=var.name)
	return(data.table(var.aggs.spk))
}

get.f0.aggs.window <- 
get.var.aggs.window <- function(x,xprops, var.name="F0") {
	currstart <- unique(x$wstarts)
        currend <- unique(x$wends)
        if (length(currend) >1) {
                print(x)
                print("Non unique end")
        }
	currconv <- unique(xprops$conv)
	currpart <- unique(xprops$participant)
	currspk <- unique(xprops$nxt_agent)

	normvar <- paste("norm", var.name,sep="")
	dvar <- paste("d",var.name,sep="") 	

        currx <- xprops[Time < currend & Time >= currstart]
	curr.aggs <- calc.spk.aggs(currx, var.name=normvar, xconv=currconv, xparticipant=currpart, xspk=currspk)

	if (length(grep(dvar, names(xprops))) > 0) {
		curr.aggs.dF <- calc.spk.aggs(currx, var.name=dvar, xconv=currconv, xparticipant=currpart, xspk=currspk)
        	xstats <- data.table(wstart=currstart, wend=currend, curr.aggs, curr.aggs.dF)
	} else { 
        	xstats <- data.table(wstart=currstart, wend=currend, curr.aggs)
	}

	slopevar <- paste("norm", var.name,".slope", sep="") 	
	if (length(grep(slopevar, names(xprops))) > 0) {
		curr.aggs.slope <- calc.spk.aggs(currx, var.name=slopevar, xconv=currconv, xparticipant=currpart, xspk=currspk)
        	xstats <- data.table(wstart=currstart, wend=currend, curr.aggs, curr.aggs.slope)
	} else { 
        	xstats <- data.table(wstart=currstart, wend=currend, curr.aggs)
	}

	if ("niteid" %in% names(x)) {
                xstats <- data.table(niteid=unique(x$niteid), xstats)
        }
        return(xstats)


} 

##----------------------------------------------------------------------------------------------
## Get intensity aggs
## This really should be generalized with the F0 stuff

## Use list structure to apply to spk sides in parallel
get.i0.aggs.all <- function(xlist, windows=NULL, wkey="xid", wsize=15) {
        i0.aggs <- mclapply(xlist, get.i0.aggs.spk, windows=windows, wkey=wkey, mc.cores=5, wsize=wsize)
        return(i0.aggs)
}

## wrapper to apply aggregation over conversations and speakers (nxt_agent) 
get.i0.aggs.spk <- function(xdt, windows=NULL, wkey="xid", wsize=15) {
	print(unique(xdt$conv))
	i0.aggs.spk <- ddply(xdt, c("conv","nxt_agent"), get.xint.windows,
			windows=windows, wkey=wkey, fx=get.i0.aggs.window, wsize=wsize)
	return(data.table(i0.aggs.spk))
}

get.f0.aggs.spk <- function(xdt, windows=NULL, wkey="xid", wsize=15) {
#	print(xdt)
	f0.aggs.spk <- ddply(xdt, c("conv","nxt_agent"), get.xint.windows,
			windows=windows, wkey=wkey, fx=get.f0.aggs.window, wsize=wsize)
	return(data.table(f0.aggs.spk))
}


get.i0.aggs.window <- function(x,xprops, wsize=15) {
	currstart <- unique(x$wstarts)
        currend <- unique(x$wends)
        if (length(currend) >1) {
                print(x)
                print("Non unique end")
        }
	currconv <- unique(xprops$conv)
	currpart <- unique(xprops$participant)
	currspk <- unique(xprops$nxt_agent)

	## Measurements in the current window  
        currx <- xprops[Time < currend & Time >= currstart]

	## get aggs
	curr.aggs <- calc.spk.aggs(currx, var.name="normI0", xconv=currconv, xparticipant=currpart, xspk=currspk)

	## If derivates exist, get their aggregates too.
	if (length(grep("dI0", names(xprops))) > 0) {
		curr.aggs.dF <- calc.spk.aggs(currx, var.name="dI0", xconv=currconv, xparticipant=currpart, xspk=currspk)
        	xstats <- data.table(wstart=currstart, wend=currend, curr.aggs, curr.aggs.dF)
	} else { 
		#print("no dy/dx")
        	xstats <- data.table(wstart=currstart, wend=currend, curr.aggs)
	}

	## Get aggs over slope corrected measurements 
	slopevar <- paste("norm", "I0", ".slope", sep="") 	
	#print(slopevar)
	if (length(grep(slopevar, names(xprops))) > 0) {
		curr.aggs.slope <- calc.spk.aggs(currx, var.name=slopevar, xconv=currconv, xparticipant=currpart, xspk=currspk)
        	xstats <- data.table(wstart=currstart, wend=currend, curr.aggs, curr.aggs.slope)
	} else { 
		#print("no normslope")
        	xstats <- data.table(wstart=currstart, wend=currend, curr.aggs)
	}

	## Add ids if not already there
	if ("niteid" %in% names(x)) {
                xstats <- data.table(niteid=unique(x$niteid), xstats)
        }
        return(xstats)


} 

##----------------------------------------------------------------------------------------------

get.f0.aggs.window <- function(x,xprops, var.name="F0") {
	#print(x)
	#print(unique(x$niteid))
	currstart <- unique(x$wstarts)
        currend <- unique(x$wends)
        if (length(currend) >1) {
                print(x)
                print("Non unique end")
        }
	currconv <- unique(xprops$conv)
	currpart <- unique(xprops$participant)
	currspk <- unique(xprops$nxt_agent)

	normvar <- paste("norm", var.name,sep="")
	dvar <- paste("d",var.name,sep="") 	

        #currx <- xprops[starttime < currend & endtime > currstart]
        currx <- xprops[Time < currend & Time >= currstart]
	curr.aggs <- calc.spk.aggs(currx, var.name=normvar, xconv=currconv, xparticipant=currpart, xspk=currspk)

	if (length(grep(dvar, names(xprops))) > 0) {
		curr.aggs.dF <- calc.spk.aggs(currx, var.name=dvar, xconv=currconv, xparticipant=currpart, xspk=currspk)
        	xstats <- data.table(wstart=currstart, wend=currend, curr.aggs, curr.aggs.dF)
	} else { 
		#print("no dy/dx")
        	xstats <- data.table(wstart=currstart, wend=currend, curr.aggs)
	}

	slopevar <- paste("norm", var.name,".slope", sep="") 	
	#print(slopevar)
	if (length(grep(slopevar, names(xprops))) > 0) {
		curr.aggs.slope <- calc.spk.aggs(currx, var.name=slopevar, xconv=currconv, xparticipant=currpart, xspk=currspk)
        	xstats <- data.table(wstart=currstart, wend=currend, curr.aggs, curr.aggs.slope)
	} else { 
		#print("no normslope")
        	xstats <- data.table(wstart=currstart, wend=currend, curr.aggs)
	}


	if ("niteid" %in% names(x)) {
                xstats <- data.table(niteid=unique(x$niteid), xstats)
        }
        return(xstats)


} 



get.f0.aggs.all <- function(xlist, windows=NULL, wkey="xid", wsize=15) {
	print(names(xlist))
        f0.aggs <- mclapply(xlist, get.f0.aggs.spk, windows=windows, wkey=wkey, mc.cores=4, wsize=wsize)
        return(f0.aggs)
}


