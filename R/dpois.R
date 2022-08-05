library(rootSolve)
#' @export
dpois <- function(n, theta, val = NULL) {
    if(!is.null(val)) {
        return (c_dpois(n, theta, val))
    } else {
        df <- data.frame(matrix(ncol = 2, nrow = 0))

        for (i in 0:n) {
            p <- c_dpois(n, theta, i)
            df <- rbind(df, c(i, p))
        }
        x <- c("val", "prob")
        colnames(df) <- x
        return (df)
    }
}

#' @export
printFinitizedPoissonDensity <- function(n, val = NULL, latex = FALSE)  {
    if(!is.null(val)) {
        x<-c_printFinitizedPoissonDensity(n, val, latex)
        NULL
    } else {
        cat(paste0("X", "\t", "pdf\n"))
        for (i in 0:n) {
            cat(paste0(i,":", '\t'))
            x<-c_printFinitizedPoissonDensity(n, i, latex)
        }
    }
}


#' @export
printFinitizedLogarithmicDensity <- function(n, val = NULL, latex = FALSE)  {
    if(!is.null(val)) {
        x<-c_printFinitizedLogarithmicDensity(n, val, latex)
        NULL
    } else {
        cat(paste0("X", "\t", "pdf\n"))
        for (i in 0:n) {
            cat(paste0(i,":", '\t'))
            x<-c_printFinitizedLogarithmicDensity(n, i, latex)
        }
    }
}


#' @export
dlog <- function(n, theta, val = NULL) {
    if(!is.null(val)) {
        return (c_dlog(n, theta, val))
    } else {
        df <- data.frame(matrix(ncol = 2, nrow = 0))

        for (i in 0:n) {
            p <- c_dlog(n, theta, i)
            df <- rbind(df, c(i, p))
        }
        x <- c("val", "prob")
        colnames(df) <- x
        return (df)
    }
}

#' @export
getLogarithmicMFPSUL <- function(nf) {

    fg<- function(theta) { x }
    body(fg)[[2]] <- parse(text = MFPS_log_pdf(nf))[[1]]
    solutions <- uniroot.all(fg, c(0,1), n = 10^6)
    return (max(solutions))
}
