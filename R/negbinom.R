#' @include utils.R
#'
#' @param n The finitization order. It should be an integer > 0.
#' @param q The parameter of the distribution
#' @param k The number of trials.
#' @param val The value of the variable for which the probability density function is computed. If NULL, a data frame containing
#' all possible values, i.e. {0 .. n}, and the corresponding probabilities is returned.
#' @export
dnegbinom <- function(n, q, k, val = NULL) {
    if (!checkFinitizationOrder(n))
        return(invisible(NULL))
    if (!checkNegBinomialQ(q))
        return(invisible(NULL))
    if (!checkNegBinomialK(k))
        return(invisible(NULL))

    if (!is.null(val)) {
        if (!checkVals(n, val))
            return(invisible(NULL))
        lim <- val
    } else {
        lim <- seq(0, n)
    }
    d <- c_d(n, lim, list("q" = q, "k" = k), getNegativeBinomialType())
    df <- data.frame(val = lim, prob = d)
    return(df)
}

#' @param n The finitization order. It should be an integer > 0.
#' @param k The number of trials.
#' @export
getNegativeBinomialMFPS <- function(n, k) {
    if(!checkFinitizationOrder(n))
        return(invisible(NULL))
    if (!checkNegBinomialK(k))
        return(invisible(NULL))

    fg <- function(q) {
        "x"
    }
    body(fg)[[2]] <- parse(text = MFPS_pdf(n, list("k" = k), getNegativeBinomialType()))[[1]]

    return(findSolutions(fg))
}

#' @param n The finitization order. It should be an integer > 0.
#' @param k The number of trials
#' @param val The value of the variable for which the probability density function is printed. If NULL, this function prints the
#' pdf for all possible values, i.e. {0 .. n}.
#' @param latex If TRUE, a string representation of the pdf formatted in Latex format is printed, otherwise it prints
#' the string representation of the pdf as an R expression.
#' @export
printFinitizedNegativeBinomialDensity <-
    function(n, k, val = NULL, latex = FALSE)  {
        if (!checkFinitizationOrder(n))
            return(invisible(NULL))
        if (!checkNegBinomialK(k))
            return(invisible(NULL))
        if (!is.null(val) && !checkVals(n, val))
            return(invisible(NULL))

        r <- printDensity(n, val, list("k" = k), getNegativeBinomialType(), latex)

        return(invisible(r))
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
rnegbinom <- function(n, q, k, no) {
    if (!checkFinitizationOrder(n))
        return(invisible(NULL))
    if (!checkNegBinomialQ(q))
        return(invisible(NULL))
    if (!checkNegBinomialK(k))
        return(invisible(NULL))
    if (!checkNoValues(no))
        return(invisible(NULL))

    return(rvalues(n, list("q" = q, "k" = k), no, getNegativeBinomialType()))
}
