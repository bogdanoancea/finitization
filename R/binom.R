#' @include utils.R

#' @param n The finitization order. It should be an integer > 0.
#'
#' @param theta The parameter of the Poisson distribution.
#'
#' @param val The value of the variable for which the probability density function is computed. If NULL, a data frame containing
#' all possible values, i.e. {0 .. n}, and the corresponding probabilities is returned.
#'
#' @export
dbinom <- function(n, p, N, val = NULL) {
    if (!checkFinitizationOrder(n))
        return(invisible(NULL))
    if (!checkBinomialP(p))
        return(invisible(NULL))
    if (!checkBinomialN(N))
        return(invisible(NULL))

    if (!is.null(val)) {
        if (!checkVals(n, val))
            return(invisible(NULL))
        lim <- val
    } else {
        lim <- seq(0, n)
    }
    d <- c_d(n, lim, list("p" = p, "N" = N), getBinomialType())
    df <- data.frame(val = lim, prob = d)
    return(df)
}

#' @param n The finitization order. It should be an integer > 0.
#' @param val The value of the variable for which the probability density function is printed. If NULL, this function prints the
#' pdf for all possible values, i.e. {0 .. n}.
#' @param latex If TRUE, a string representation of the pdf formatted in Latex format is printed, otherwise it prints
#' the string representation of the pdf as an R expression.
#' @export
printFinitizedBinomialDensity <-
    function(n, N, val = NULL, latex = FALSE)  {
        if (!checkFinitizationOrder(n))
            return(invisible(NULL))
        if (!checkBinomialN(N))
            return(invisible(NULL))
        if (!is.null(val) && !checkVals(n, val))
            return(invisible(NULL))

        r <- printDensity(n, val, list("N" = N), getBinomialType(), latex)

        return(invisible(r))
    }

#' @param  n The finitization order. It should be an integer > 0.
#' @param N The number of trials
#' @export
getBinomialMFPS <- function(n, N) {
    if(!checkFinitizationOrder(n))
        return(invisible(NULL))
    if (!checkBinomialN(N))
        return(invisible(NULL))

    fg <- function(p) {
        "x"
    }
    body(fg)[[2]] <- parse(text = MFPS_pdf(n, list("N" = N), getBinomialType()))[[1]]

    return(findSolutions(fg))
}

#' Title
#'
#' @param n
#' @param p
#' @param N
#' @param no
#'
#' @return
#' @export
#'
#' @examples
rbinom <- function(n, p, N, no) {
    if (!checkFinitizationOrder(n))
        return(invisible(NULL))
    if (!checkBinomialP(p))
        return(invisible(NULL))
    if (!checkBinomialN(N))
        return(invisible(NULL))
    if (!checkNoValues(no))
        return(invisible(NULL))

    return(rvalues(n, list("p" = p, "N" = N), no, getBinomialType()))
}











