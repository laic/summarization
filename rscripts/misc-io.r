read.mlf.files <- function(recdir) {
        filenames <- list.files(recdir, pattern="*.mlf.melt")
        recx <- data.table()
        for (filename in filenames) {
                currfile <- paste(recdir, filename, sep="/")
                print(currfile)
                x <- data.table(read.table(currfile))
                recx <- rbind(recx, x)

        }
        setnames(recx, c("segid","conv","spk","seg.start","seg.end","starttime","endtime","trigram","word"))
        recx$seg.start <- recx$seg.start/100
        recx$seg.end  <- recx$seg.end/100
        recx$starttime <- recx$starttime * 10^(-7) + recx$seg.start
        recx$endtime <- recx$endtime * 10^(-7) + recx$seg.start

        return(recx)
}

