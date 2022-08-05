
#' @param n The finitization order. It should be an integer > 0.
#' @param val The value of the variable for which the probability density function is computed. If NULL, a data frame containing
#' all possible values, i.e. {0 .. n}, and the corresponding probabilities is returned.
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

#' @param n The finitization order. It should be an integer > 0.
#' @export
getLogarithmicMFPSUL <- function(n) {

    fg<- function(theta) { "x" }
    body(fg)[[2]] <- parse(text = MFPS_log_pdf(n))[[1]]
    solutions <- rootSolve::uniroot.all(fg, c(0,1), n = 10^6)
    return (max(solutions))
}

#' @param n The finitization order. It should be an integer > 0.
#' @param val The value of the variable for which the probability density function is printed. If NULL, this function prints the
#' pdf for all possible values, i.e. {0 .. n}.
#' @param latex If TRUE, a string representation of the pdf formatted in Latex format is printed, otherwise it prints
#' the string representation of the pdf as an R expression.
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
