#!/usr/bin/Rscript --vanilla --slave

library(data.table)
library(tm)

merge.word.feats <- function(x.lex.grp, x.lex.f0.dt) {
	x <- x.lex.grp[,list(niteid, clean.word, tf.idf, su.idf, tf.idf.spk, idf, idf.spk)] 
	setkey(x, niteid) 
	setkey(x.lex.f0.dt, niteid)
        f0.x <- x.lex.f0.dt[x]

        return(f0.x)
}



###################################################################################
args=(commandArgs(TRUE))
print(args)
if(length(args)<3){
	stop("Not enough arguments supplied.")
} else {
	currconv <- args[1]
	featname <- args[2]
	segsdir <- args[3]
	rmstop <- as.logical(args[4])
	wtype <- args[5]

	if (wtype=="manual") {
		wtype <- ""
	}

	## Load the data
	aggs.suffix <- paste(".aggs.",wtype,"word.txt", sep="") 
	lextype <- paste(wtype,"lex",sep="")

	prosfile <- paste(segsdir,"/f0/", currconv, aggs.suffix, sep="")		
	x.lex.f0 <- data.table(read.table(prosfile, header=TRUE)) 		

	prosfile <- paste(segsdir,"/i0/", currconv, aggs.suffix, sep="")		
	x.lex.i0 <- data.table(read.table(prosfile, header=TRUE)) 		

	lexfile <- paste(segsdir, "/", lextype, "/", currconv, ".lex.grp", sep="")		
	xlex <- load(lexfile) 		
	x.lex.grp <- data.table(get(xlex)) 

	## Fix previous naming inconsistencies
	if (!("niteid" %in% names(x.lex.grp))) {
		setnames(x.lex.grp, c("id"), c("niteid"))
	}

	if ("tf.grp" %in% names(x.lex.grp)) {
		setnames(x.lex.grp, c("tf","idf","tf.idf"), c("tf.spk","idf.spk","tf.idf.spk")) 
		setnames(x.lex.grp, c("tf.grp","idf.grp","tf.idf.grp"), c("tf","idf","tf.idf")) 
	}
	
	if (rmstop ==T) {	
		print("rmstop")
		x.lex.grp$tf.idf[x.lex.grp$clean.word %in% stopwords()] <- 0 
		x.lex.grp$su.idf[x.lex.grp$clean.word %in% stopwords()] <- 0 
	}

	## Join data.tables 	
        setkey(x.lex.f0, niteid, wstart, wend, conv, participant, nxt_agent)
        setkey(x.lex.i0, niteid, wstart, wend, conv, participant, nxt_agent)

	x.lex.if0 <- x.lex.f0[x.lex.i0] 

        setkey(x.lex.grp, niteid, conv)
	setkey(x.lex.if0, niteid, conv)  

	x.tf <- merge.word.feats(x.lex.grp, x.lex.if0) 	

	## PMI vals: this is a label dependency
	pmifile <- paste(segsdir, "/", lextype, "/", currconv, ".pmi.txt", sep="")		
	x.pmi <- data.table(read.table(pmifile, header=TRUE)) 		

	if (!("link.eda") %in% names(x.pmi)) {
		print("try to add link eda")
		edafile <- paste(segsdir,"/extractive/", currconv, ".abs.multi.txt", sep="")		
		x.eda <- try(data.table(read.table(edafile, header=TRUE))) 		
		if(is.data.table(x.eda)) {
			x.pmi <- data.table(x.pmi[,list(link.eda=(niteid %in% x.eda$niteid))], x.pmi)
		}
	} else {
		x.pmi <- x.pmi[,list(link.eda, niteid, pmi_t, pmi_f, wfreq)]
	}

	
	setkey(x.tf, niteid)
	setkey(x.pmi, niteid)
	x.tf <- x.pmi[x.tf]
	print(c(nrow(x.tf), nrow(x.pmi)))

	outfile.txt <- paste(segsdir, "/", lextype, "/", currconv, ".tf.allpros.txt", sep="")		
	write.table(x.tf, file=outfile.txt, row.names=F, quote=T)	
	
}


print("END")

