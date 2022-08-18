#' The density for the Logarithmic distribution.
#'
#' \code{dlog(n, theta, val)} computes the finitized Logarithmic density for \code{val}.
#'
#' @param n The finitization order. It should be an integer > 0.
#' @param theta The parameter of the finitized Logarithmic distribution.
#' @param val A vector with the values of the variable for which the probability density is computed. If NULL, a data frame containing
#' all possible values, i.e. \code{{0 .. n}}, and the corresponding probabilities is returned.
#' @return a \code{data.frame} object with two columns: \code{val} containing the values passed in parameter \code{val} and \code{prob}
#' containing the corresponding densities.
#'
#' @examples
#' library(finitization)
#' dlog(4, 0.5, c(0,1,3))
#'
#' @include utils.R
#' @export
dlog <- function(n, theta, val = NULL) {
    if(missing(n)) {
        message("Argument n is missing!\n")
        return(invisible(NULL))
    }
    if(missing(theta)) {
        message("Argument theta is missing!\n")
        return(invisible(NULL))
    }
    if (!checkIntegerValue(n))
        return(invisible(NULL))
    if (!checkTheta(theta))
        return(invisible(NULL))
    if (!is.null(val)) {
        if (!checkVals(n, val))
            return(invisible(NULL))
        lim <- val
    } else {
        lim <- seq(0, n)
    }
    d <- c_d(n, lim, list("theta" = theta), getLogarithmicType())
    if(any(d < 0) || any(d > 1))
        warning(paste0("Be sure that you provided parameter ", theta, " inside the maximum feasible parameter space"))

    df <- data.frame(val = lim, prob = d)
    return(df)
}

#' Maximum feasible parameter space  for the finitized Logarithmic distribution.
#'
#' \code{getLogarithmicMFPS(n)} computes and returns the maximum feasible parameter space for the finitized Logarithmic distribution.
#'
#' @param  n The finitization order. It should be an integer > 0.
#' @return A vector with two elements where the first element is the lower limit of the maximum feasible parameter space
#' and the second is the upper limit.
#'
#' @examples
#' library(finitization)
#' getLogarithmicMFPS(4)
#'
#' @include utils.R
#' @export
getLogarithmicMFPS <- function(n) {
    if(missing(n)) {
        message("Argument n is missing!\n")
        return(invisible(NULL))
    }
    if(!checkIntegerValue(n))
        return(invisible(NULL))

    fg <- function(theta) {
        "x"
    }
    body(fg)[[2]] <- parse(text = MFPS_pdf(n, NULL, getLogarithmicType()))[[1]]

    return(findSolutions(fg))
}


#' The string representation of the probability density function for the finitized Logarithmic distribution.
#'
#' \code{printFinitizedLogarithmicDensity(n, val, latex)} computes and prints the string
#' representation of the probability density function for the finitized Logarithmic distribution.
#'
#' @param n The finitization order. It should be an integer > 0.
#' @param val The value of the variable for which the probability density function is printed. If NULL, this function computes the
#' string representation of the pdf for all possible values, i.e. \code{{0 .. n}}.
#' @param latex If TRUE, a string representation of the pdf formatted in Latex format is printed, otherwise this function prints
#' the string representation of the pdf as an R expression.
#' @return This function silently returns a vector of type \code{\link[base]{character}} with the string representation of the pdf(s).
#' The length of the vector is the same with the length of the parameter \code{val}, i.e. one element for each value in \code{val}.
#'
#' @examples
#' library(finitization)
#' printFinitizedLogarithmicDensity(4)
#'
#' @include utils.R
#' @export
printFinitizedLogarithmicDensity <-
    function(n, val = NULL, latex = FALSE)  {
        if (!checkIntegerValue(n))
            return(invisible(NULL))
        if (!is.null(val) && !checkVals(n, val))
            return(invisible(NULL))

        r <- printDensity(n, val, NULL, getLogarithmicType(), latex)
        return(invisible(r))
    }

#' Random values generation  for the finitized Logarithmic distribution.
#'
#' \code{rlog(n, theta, no)} generates random values according to the finitized Logarithmic distribution with parameter \code{theta}.
#'
#' @param n The finitization order. It should be an integer > 1.
#' @param theta The parameter of the Logarithmic distribution.
#' @param no The number of random values to be generated.
#'
#' @return \code{rlog} returns a vector of type \code{\link[base]{integer}} containing random values generated according to the finitized Logarithmic distribution.
#' The number of values is given by the parameter \code{no}.
#'
#' @examples
#' library(finitization)
#' rlog(2, 0.25, 10)
#'
#' @include utils.R
#' @export
rlog <- function(n, theta, no) {
    if(missing(n)) {
        message("Argument n is missing!\n")
        return(invisible(NULL))
    }
    if(missing(theta)) {
        message("Argument theta is missing!\n")
        return(invisible(NULL))
    }
    if(missing(no)) {
        message("Argument no is missing!\n")
        return(invisible(NULL))
    }

    if (!checkIntegerValue(n))
        return(invisible(NULL))
    if (!checkTheta(theta))
        return(invisible(NULL))
    if (!checkIntegerValue(no))
        return(invisible(NULL))

    return(rvalues(n, list("theta" = theta), no, getLogarithmicType()))
}
