## Helper functions for calculating lexical term-frequency features.
## Used in: get-tf-feats.r 

source("../rscripts/basics.r")
source("../rscripts/nxt-proc.r")
source("../rscripts/term-freq.r")
library(plyr)
library(tm)

## Calculate various term frequency features
collect.tf.feats <- function(x.nxtwords.dt0, word.idf.spk, word.idf.conv, outdir="./", prefix="x") { 

	## tf.idf.spk: each speaker side as a separate document.
	print("tf.idf.spk")
        x.tf.idf <- get.tfidf.spk(x.nxtwords.dt0, word.idf=word.idf.spk)
	print(names(x.tf.idf))

	print("-----")
	print("tf.idf.grp")
        x.tf.idf.grp <- get.tfidf.grp(x.nxtwords.dt0, word.idf=word.idf.conv)
	print(names(x.tf.idf.grp))

	print("-----")

	print("su.idf")
        x.su.idf <- get.su.idf(x.nxtwords.dt0, x.tf.idf)

	## add tf.idf.spk to transcript words 
	setnames(x.tf.idf, c("tcount","tf","idf","tf.idf"), c("tcount.spk","tf.spk","idf.spk","tf.idf.spk"))
        setkey(x.nxtwords.dt0, conv, spk, clean.word)
        setkey(x.tf.idf, conv, spk, clean.word)
        x.lex <- x.tf.idf[x.nxtwords.dt0][!is.na(tf.spk)]

	## add su.idf
        setkey(x.lex, conv, clean.word)
        setkey(x.su.idf, conv, clean.word)
        x.lex.su <- x.su.idf[x.lex]

	## add tf.idf.grp 
        setkey(x.lex.su, conv, clean.word)
        setkey(x.tf.idf.grp, conv, clean.word)
	print("-----")
	print("-----")
	print("-----")
	print(names(x.lex.su))
	print("-----")
	print(names(x.tf.idf.grp))

        x.lex.grp <- x.tf.idf.grp[x.lex.su][!is.na(tf.spk)]
	print("-----")
	print(names(x.lex.grp))
	print("-----")
	print(gsub(".1$", ".spk", names(x.lex.grp)))
	print("-----")

        setnames(x.lex.grp, names(x.lex.grp), gsub(".1$", ".spk", names(x.lex.grp)))

	return(x.lex.grp)

}

##--------------------------------------------------------------------------
## Calculate PMI values: x.word.eda is data.table with columns link.eda is T if that 
## DA is a linked EDA.

get.eda.pmi <- function(x.word.eda) {
	N <- nrow(x.word.eda)
	p_t <- nrow(x.word.eda[link.eda==T])/N
	p_f <- nrow(x.word.eda[link.eda==F])/N
	eda.pmi <- x.word.eda[,{
		n.eda <- sum(link.eda==T)
		n.noneda <- sum(link.eda==F)
		n.words <- length(link.eda) 
		list(n.eda=n.eda, n.noneda=n.noneda, 
		n.words=n.words, 
		pmi_t = log((n.eda/n.words)/p_t), 
		pmi_f = log((n.noneda/n.words)/p_f), 
		wfreq = n.words/N
	)},by=list(clean.word)]

	return(eda.pmi)
}

## Fix PMI data, so that missing values are set to zero
## Words that never appear in a categories are set to the observed
## minimum PMI value over the data set.
clean.pmi <- function(x.pmi) {
	x.pmi$n.eda[is.na(x.pmi$n.eda)] <- 0
        x.pmi$n.noneda[is.na(x.pmi$n.noneda)] <- 0
        x.pmi$n.words[is.na(x.pmi$n.words)] <- 0
        x.pmi$pmi_t[is.na(x.pmi$pmi_t)] <- 0
        x.pmi$pmi_f[is.na(x.pmi$pmi_f)] <- 0
        x.pmi$wfreq[is.na(x.pmi$wfreq)] <- 0
        x.pmi$pmi_t[is.infinite(x.pmi$pmi_t)] <- min(x.pmi$pmi_t[!is.infinite(x.pmi$pmi_t)])
        x.pmi$pmi_f[is.infinite(x.pmi$pmi_f)] <- min(x.pmi$pmi_f[!is.infinite(x.pmi$pmi_f)])
	return(x.pmi)

}

##############################################################################################
# To remember...
##############################################################################################

get.ami.manual.transcripts <- function(conv, 
	spurtdir="~/data/ami/derived/spurts/"
	) 
{
        ## Get word into one data.table.
        #nxtwords.dt0 <- get.ami.words(dirname="~/data/ami/Data/AMI/NXT-format/words/", 
        #                outfile="~/data/ami/derived/ami.nxtwords.dt0")

        words.dt <- get.ami.words.conv(conv, dirname="~/data/ami/Data/AMI/NXT-format/words/")

	#-- word time --#
	print("#-- word time --#")
        ## write out word times: x.conv.word
        write.conv.seg(words.dt[.id == "w"], dirname="~/data/ami/derived/word/", segname="word")

	setnames(words.dt, c("starttime", "endtime"), c("wstart","wend"))
        write.features.by.conv(words.dt[.id == "w"], dirname="~/data/ami/derived/word/", fsuffix=".raw.word", plain.txt=F)
        #write.features.by.conv(words.dt[.id != "w"], dirname="~/data/ami/derived/word/", fsuffix=".raw.nonword", plain.txt=T)
	
	print("#-- spurts --#")
        ## Get spurts 
	setnames(words.dt, c("wstart","wend"), c("starttime", "endtime"))
        spurts.dt <- get.spurts.from.words(words.dt)
        spurts.dt <- add.ami.channel.info(spurts.dt)
        spurts.dt <- data.table(niteid=spurts.dt[,paste(conv,spk,participant,sid,sep=".")], spurts.dt)
		
	spurts.dt <- spurts.dt[,{
                list(conv=conv, spk=spk, participant=participant, sid=sid, chno=channel, vidsrc="ami",
                starttime=starttime, endtime=endtime, niteid=niteid,
                longconv=conv, wav.file=paste(conv, ".Headset-", channel, ".wav", sep=""), video.file=NA
                )}]

        write.features.by.conv(spurts.dt, dirname=spurtdir, fsuffix=".spurts", plain.txt=T)

	print("#-- utts = DAs from NXT query --#")
	da.dt <- get.das.conv(conv)
	print("HERE")
        write.conv.seg(da.dt, dirname="~/data/ami/derived/da/", segname="da")
}


## A generalized version
main.manual.tf <- function(nxtwords.dt0, lexdir="~/data/ami/derived/lex/", deriveddir="~/data/ami/derived/", 
		prefix="ami", corpus="ami", pattern="[EIT]S") {

 #       load("~/data/ami/derived/ami.asr.nxtwords.dt0")

        nxtwords.dt0$clean.word <- clean.stem.word(nxtwords.dt0$word, stem=T, remove.morph=T)

        ## get tf.idf.spk, su.idf, tf.idf.grp and associated features
        x.lex.grp <- collect.tf.feats(nxtwords.dt0, word.idf.spk=word.idf.spk, word.idf.conv=word.idf.conv)

        write.table(x.lex.grp, file=paste(deriveddir, "/", prefix, ".lex.grp.txt", sep=""))
        write.features.by.conv(x.lex.grp, dirname=lexdir, fsuffix=".lex.grp")

        ## PMI features ####
        x.words <- x.lex.grp[,list(conv, spk, niteid, word, clean.word, starttime, endtime)]

        ## Get EDA times
        abs.names <- c("abs.id", "s.id", "slink.id", "niteid", "starttime","endtime","da.type")
	absfile <- paste(deriveddir, "/", prefix, ".abs.da.txt", sep="")
        x.abs <- get.abs.ext.dt(filename=absfile, corpus=corpus, abs.names=abs.names)
        x.eda <- x.abs[,list(conv, spk=get.spk.from.daid(niteid), niteid, starttime, endtime)]
	
	x.word.eda <- project.eda.to.word.all(x.eda, x.words)
        write.table(x.word.eda, file=paste(deriveddir, "/", prefix, ".word.eda.txt", sep=""))
        write.table(x.word.eda[grep("^[EIT]S", conv)], file=paste(deriveddir, "/", prefix, ".word.eda.txt", sep=""))


        ## Calculate PMI features
        eda.pmi <- get.eda.pmi(x.word.eda[!(conv %in% c(test.convs, dev.convs))])
	
        #eda.pmi <- data.table(eda.pmi, wfreq=eda.pmi[,n.words/sum(eda.pmi$n.words)])

        write.table(eda.pmi, file=paste(deriveddir, "/", prefix, "eda.pmi.txt", sep=""))

        setkey(eda.pmi, clean.word)
        setkey(x.word.eda, clean.word)
        x.pmi <- eda.pmi[x.word.eda][grep(pattern, conv)]

        # Fix words that aren't in the training set.
        x.pmi.clean <- clean.pmi(x.pmi)

        write.table(x.pmi.clean, file=paste(deriveddir, "/", prefix, ".pmi.all.txt", sep=""))

        write.features.by.conv(x.pmi.clean, dirname=lexdir, fsuffix=".pmi")
}



