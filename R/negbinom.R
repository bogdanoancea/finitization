#' The density for the finitized Negative Binomial distribution.
#'
#' \code{dbinom(n, q, k, val)} computes the finitized Negative Binomial density for \code{val}.
#'
#' @param n The finitization order. It should be an integer > 0.
#' @param q The parameter of the finitized Negative Binomial distribution - the success probability for each trial.\eqn{q \in [0,1]}
#' @param k The number of failures until the experiment is stopped,\code{k > 0}.
#' @param val A vector with the values of the variable for which the probability density is computed. If NULL, a data frame containing
#' all possible values, i.e. \code{{0 .. n}}, and the corresponding probabilities is returned.
#' @return a \code{data.frame} object with two columns: \code{val} containing the values passed in parameter \code{val} and \code{prob}
#' containing the corresponding densities.
#'
#' @examples
#' library(finitization)
#' dnegbinom(4, 0.12, 4)
#'
#' @include utils.R
#' @export
dnegbinom <- function(n, q, k, val = NULL) {

    if(missing(n)) {
        message("Argument n is missing!\n")
        return(invisible(NULL))
    }
    if(missing(q)) {
        message("Argument q is missing!\n")
        return(invisible(NULL))
    }
    if(missing(k)) {
        message("Argument k is missing!\n")
        return(invisible(NULL))
    }

    if (!checkIntegerValue(n))
        return(invisible(NULL))
    if (!checkNegBinomialQ(q))
        return(invisible(NULL))
    if (!checkIntegerValue(k))
        return(invisible(NULL))

    if (!is.null(val)) {
        if (!checkVals(n, val))
            return(invisible(NULL))
        lim <- val
    } else {
        lim <- seq(0, n)
    }
    d <- c_d(n, lim, list("q" = q, "k" = k), getNegativeBinomialType())
    if(any(d < 0) || any(d > 1))
        warning(paste0("Be sure that you provided parameter ", q, " inside the maximum feasible parameter space"))

    df <- data.frame(val = lim, prob = d)
    return(df)
}

#' Maximum feasible parameter space  for the finitized Negative Binomial distribution.
#'
#' \code{getNegativeBinomialMFPS(n, k)} computes and returns the maximum feasible parameter space for the finitized Negative
#' Binomial distribution with parameter \code{k}.
#'
#' @param n The finitization order. It should be an integer > 0.
#' @param k The number of failures until the experiment is stopped,\code{k > 0}.
#' @return A vector with two elements where the first element is the lower limit of the maximum feasible parameter space
#' and the second is the upper limit.
#'
#' @examples
#' library(finitization)
#' getNegativeBinomialMFPS(2, 4)
#'
#' @include utils.R
#' @export
getNegativeBinomialMFPS <- function(n, k) {
    if(missing(n)) {
        message("Argument n is missing!\n")
        return(invisible(NULL))
    }
    if(missing(k)) {
        message("Argument k is missing!\n")
        return(invisible(NULL))
    }

    if (!checkIntegerValue(n))
        return(invisible(NULL))
    if (!checkIntegerValue(k))
        return(invisible(NULL))

    fg <- function(q) {
        "x"
    }
    body(fg)[[2]] <- parse(text = MFPS_pdf(n, list("k" = k), getNegativeBinomialType()))[[1]]

    return(findSolutions(fg))
}

#' The string representation of the probability density function for the finitized Negative Binomial distribution.
#'
#' \code{printFinitizedNegativeBinomialDensity(n,k, val, latex)} computes and prints the string
#' representation of the probability density function for the finitized Negative Binomial distribution with parameter \code{k}.
#'
#' @param n The finitization order. It should be an integer > 0.
#' @param k The number of failures until the experiment is stopped,\code{k > 0}.
#' @param val The value of the variable for which the probability density function is printed. If NULL, this function computes the
#' string representation of the pdf for all possible values, i.e. \code{{0 .. n}}.
#' @param latex If TRUE, a string representation of the pdf formatted in Latex format is printed, otherwise this function prints
#' the string representation of the pdf as an R expression.
#' @return This function silently returns a vector of type \code{\link[base]{character}} with the string representation of the pdf(s).
#' The length of the vector is the same with the length of the parameter \code{val}, i.e. one element for each value in \code{val}.
#'
#' @examples
#' library(finitization)
#' printFinitizedNegativeBinomialDensity(4, 4)
#'
#' @include utils.R
#' @export
printFinitizedNegativeBinomialDensity <-
    function(n, k, val = NULL, latex = FALSE)  {
        if(missing(n)) {
            message("Argument n is missing!\n")
            return(invisible(NULL))
        }
        if(missing(k)) {
            message("Argument k is missing!\n")
            return(invisible(NULL))
        }
        if (!checkIntegerValue(n))
            return(invisible(NULL))
        if (!checkIntegerValue(k))
            return(invisible(NULL))
        if (!is.null(val) && !checkVals(n, val))
            return(invisible(NULL))

        r <- printDensity(n, val, list("k" = k), getNegativeBinomialType(), latex)

        return(invisible(r))
    }

#' Random values generation for the finitized Negative Binomial distribution.
#'
#' \code{rnegbinom(n, q, k, no)} generates random values according to the finitized Binomial distribution with parameters \code{q,k}.
#'
#' @param n The finitization order. It should be an integer > 1.
#' @param q The parameter of the finitized Negative Binomial distribution - the success probability for each trial.\eqn{q \in [0,1]}
#' @param k The number of failures until the experiment is stopped,\code{k > 0}.
#' @param no The number of random values to be generated.
#'
#' @return \code{rpois} returns a vector of type \code{\link[base]{integer}} containing random values generated according to the finitized Negative
#' Binomial distribution. The number of values is given by the parameter \code{no}.
#'
#' @examples
#' library(finitization)
#' rnegbinom(2, 0.5, 2, 10)
#'
#' @include utils.R
#' @export
rnegbinom <- function(n, q, k, no) {
    if(missing(n)) {
        message("Argument n is missing!\n")
        return(invisible(NULL))
    }
    if(missing(q)) {
        message("Argument q is missing!\n")
        return(invisible(NULL))
    }
    if(missing(k)) {
        message("Argument k is missing!\n")
        return(invisible(NULL))
    }
    if(missing(no)) {
        message("Argument no is missing!\n")
        return(invisible(NULL))
    }

    if (!checkIntegerValue(n))
        return(invisible(NULL))
    if (!checkNegBinomialQ(q))
        return(invisible(NULL))
    if (!checkIntegerValue(k))
        return(invisible(NULL))
    if (!checkIntegerValue(no))
        return(invisible(NULL))

    return(rvalues(n, list("q" = q, "k" = k), no, getNegativeBinomialType()))
}
