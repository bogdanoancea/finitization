
#' @param n The finitization order. It should be an integer > 0.
#' @param val The value of the variable for which the probability density function is computed. If NULL, a data frame containing
#' all possible values, i.e. {0 .. n}, and the corresponding probabilities is returned.
#' @export
dlog <- function(n, theta, val = NULL) {
    if (!checkFinitizationOrder(n))
        return(invisible(NULL))
    if (!checkLogarithmicTheta(theta))
        return(invisible(NULL))
    if (!is.null(val)) {
        if (!checkVals(n, val))
            return(invisible(NULL))
        lim <- val
    } else {
        lim <- seq(0, n)
    }
    d <- c_d(n, lim, list("theta" = theta), getLogarithmicType())
    df <- data.frame(val = lim, prob = d)
    return(df)
}

#' @param n The finitization order. It should be an integer > 0.
#' @export
getLogarithmicMFPS <- function(n) {
    if(!checkFinitizationOrder(n))
        return(invisible(NULL))

    fg <- function(theta) {
        "x"
    }
    body(fg)[[2]] <- parse(text = MFPS_pdf(n, NULL, getLogarithmicType()))[[1]]

    return(findSolutions(fg))
}


#' @param n The finitization order. It should be an integer > 0.
#' @param val The value of the variable for which the probability density function is printed. If NULL, this function prints the
#' pdf for all possible values, i.e. {0 .. n}.
#' @param latex If TRUE, a string representation of the pdf formatted in Latex format is printed, otherwise it prints
#' the string representation of the pdf as an R expression.
#' @export
printFinitizedLogarithmicDensity <-
    function(n, val = NULL, latex = FALSE)  {
        if (!checkFinitizationOrder(n))
            return(invisible(NULL))
        if (!is.null(val) && !checkVals(n, val))
            return(invisible(NULL))

        r <- printDensity(n, val, NULL, getLogarithmicType(), latex)
        return(invisible(r))
    }

#' Title
#'
#' @param n
#' @param theta
#' @param no
#'
#' @return
#' @export
#'
#' @examples
rlog <- function(n, theta, no) {
    if (!checkFinitizationOrder(n))
        return(invisible(NULL))
    if (!checkLogarithmicTheta(theta))
        return(invisible(NULL))
    if (!checkNoValues(no))
        return(invisible(NULL))
    return(rvalues(n, list("theta" = theta), no, getLogarithmicType()))
}
