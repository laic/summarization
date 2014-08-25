regroup <- function(fx, vs, vname) {
        xf0 <- list()
        for (v in vs) {
                xf0[[v]] <- list()
        }
        for (i in c(1:length(fx))){
                x <- fx[[i]]
                if (is.factor(x[[vname]])) {
                        curr <- unlevel(x[[vname]][1])
                } else {
                        curr <- x[[vname]][1]
                }
                print(curr)
                xf0[[curr]][[unlevel(x$fstem[1])]] <- x
                print(unlevel(x$fstem[1]))
        }

        return(xf0)
}

