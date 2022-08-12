#' @include utils.R
#'
#' @param n The finitization order. It should be an integer > 0.
#' @param q The parameter of the distribution
#' @param k The number of trials.
#' @param val The value of the variable for which the probability density function is computed. If NULL, a data frame containing
#' all possible values, i.e. {0 .. n}, and the corresponding probabilities is returned.
#' @export
dnegbinom <- function(n, q, k, val = NULL) {
    if (!is.null(val)) {
        return(c_dnegbinom(n, q, k, val))
    } else {
        df <- data.frame(matrix(ncol = 2, nrow = 0))

        for (i in 0:n) {
            r <- c_dnegbinom(n, q, k, i)
            df <- rbind(df, c(i, r))
        }
        x <- c("val", "prob")
        colnames(df) <- x
        return(df)
    }
}

#' @param n The finitization order. It should be an integer > 0.
#' @param k The number of trials.
#' @export
getNegativeBinomialMFPS <- function(n, k) {
    fg <- function(q) { "x" }
    body(fg)[[2]] <- parse(text = MFPS_negbinom_pdf(n, k))[[1]]
    U <- 1
    L <- 0

    while (is.infinite(fg(U)) )
        U <- U - .Machine$double.eps
    while (is.infinite(fg(L)) )
        L <- L + .Machine$double.eps
    solutions <- rootSolve::uniroot.all(fg, c(U, L), n = 10^7, tol = .Machine$double.eps)
    UL = solutions[length(solutions)]
    if(length(solutions) > 1)
        LL = solutions[length(solutions) - 1]
    else
        LL = 0
    return(c(min(LL,UL), max(LL,UL)))
}

#' @param n The finitization order. It should be an integer > 0.
#' @param k The number of trials
#' @param val The value of the variable for which the probability density function is printed. If NULL, this function prints the
#' pdf for all possible values, i.e. {0 .. n}.
#' @param latex If TRUE, a string representation of the pdf formatted in Latex format is printed, otherwise it prints
#' the string representation of the pdf as an R expression.
#' @export
printFinitizedNegativeBinomialDensity <- function(n, k, val = NULL, latex = FALSE)  {
    if(n < 1) {
        cat("n shlould be an integer > 0\n")
    } else {
        params<-list()
        params["k"] = k
        printDensity(n, val, params, getNegativeBinomialType(), latex)
    }
}
