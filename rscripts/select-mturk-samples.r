

filenames <- list.files("./", pattern="TED.*tf.pros_pros.autosent0.7.eval.txt")

file.samp <- filenames[sample.int(20)]

outdir="~/mturk/"
outfile="~/mturk/quote.csv"
y <- NULL
for (filename in file.samp) {
	print(filename)
	x <- data.table(read.table(filename, header=T))
	x1 <- c(1,1,1,5,5,10)
	x2 <- c(5,10,20,10,20,20)

	u  <- sample(c(0,1),size=6, replace=T)
	y1 <- x1
	y2 <- x2

	y1[u==1] <- x2[u==1]
	y2[u==1] <- x1[u==1]

	data.table(y1, y2)


	y0 <- data.table(conv=x$conv[1], firstid=x$niteid[y1], secondid=x$niteid[y2], 
			firstquote=x$trans[y1], secondquote=x$trans[y2], 
			firstlval=x$logit.val[y1], secondlval=x$logit.val[y2] )
	y <- rbindlist(list(y,y0))
}

y <- y[sample.int(nrow(y))]

write.csv(y, file=outfile, row.names=F)

