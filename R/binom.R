#' The density for the finitized Binomial distribution.
#'
#' \code{dbinom(n, p, N, val)} computes the finitized Binomial density for \code{val}.
#'
#' @param n The finitization order. It should be an integer > 0.
#' @param p The parameter of the finitized Binomial distribution - the success probability for each trial.\eqn{p \in [0,1]}
#' @param N The number of trials.\code{ n = 0, 1, 2, ...}
#' @param val A vector with the values of the variable for which the probability density is computed. If NULL, a data frame containing
#' all possible values, i.e. \code{{0 .. n}}, and the corresponding probabilities is returned.
#' @return a \code{data.frame} object with two columns: \code{val} containing the values passed in parameter \code{val} and \code{prob}
#' containing the corresponding densities.
#'
#' @examples
#' library(finitization)
#' dbinom(4, 0.5, 4)
#'
#' @include utils.R
#' @export
dbinom <- function(n, p, N, val = NULL) {
    if(missing(n)) {
        message("argument n is missing!\n")
        return(invisible(NULL))
    }
    if(missing(p)) {
        message("argument p is missing!\n")
        return(invisible(NULL))
    }
    if(missing(N)) {
        message("argument N is missing!\n")
        return(invisible(NULL))
    }

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
    if(any(d < 0) || any(d > 1))
        warning("be sure that you provided parameters inside the maximum feasible parameter space")

    df <- data.frame(val = lim, prob = d)
    return(df)
}


#' The string representation of the probability density function for the finitized Binomial distribution.
#'
#' \code{printFinitizedBinomialDensity(n, N, val, latex)} computes and prints the string
#' representation of the probability density function for the finitized Binomial distribution with parameter \code{N}.
#'
#' @param n The finitization order. It should be an integer > 0.
#' @param N The number of trials.\code{ n = 0, 1, 2, ...}
#' @param val The value of the variable for which the probability density function is printed. If NULL, this function computes the
#' string representation of the pdf for all possible values, i.e. \code{{0 .. n}}.
#' @param latex If TRUE, a string representation of the pdf formatted in Latex format is printed, otherwise this function prints
#' the string representation of the pdf as an R expression.
#' @return This function silently returns a vector of type \code{\link[base]{character}} with the string representation of the pdf(s).
#' The length of the vector is the same with the length of the parameter \code{val}, i.e. one element for each value in \code{val}.
#'
#' @examples
#' library(finitization)
#' printFinitizedBinomialDensity(4)
#'
#' @include utils.R
#' @export
printFinitizedBinomialDensity <-
    function(n, N, val = NULL, latex = FALSE)  {
        if(missing(n)) {
            message("argument n is missing!\n")
            return(invisible(NULL))
        }
        if(missing(N)) {
            message("argument N is missing!\n")
            return(invisible(NULL))
        }
        if (!checkFinitizationOrder(n))
            return(invisible(NULL))
        if (!checkBinomialN(N))
            return(invisible(NULL))
        if (!is.null(val) && !checkVals(n, val))
            return(invisible(NULL))

        r <- printDensity(n, val, list("N" = N), getBinomialType(), latex)

        return(invisible(r))
    }

#' Maximum feasible parameter space  for the finitized Binomial distribution.
#'
#' \code{getBinomialMFPS(n, N)} computes and returns the maximum feasible parameter space for the finitized Binomial distribution with parameter
#' \code{N}.
#'
#' @param n The finitization order. It should be an integer > 0.
#' @param N The number of trials.\code{ n = 0, 1, 2, ...}
#' @return A vector with two elements where the first element is the lower limit of the maximum feasible parameter space
#' and the second is the upper limit.
#'
#' @examples
#' library(finitization)
#' getBinomialMFPS(2, 4)
#'
#' @include utils.R
#' @export
getBinomialMFPS <- function(n, N) {
    if(missing(n)) {
        message("argument n is missing!\n")
        return(invisible(NULL))
    }
    if(missing(N)) {
        message("argument N is missing!\n")
        return(invisible(NULL))
    }

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


#' Random values generation for the finitized Binomial distribution.
#'
#' \code{rbinom(n, p, N, no)} generates random values according to the finitized Binomial distribution with parameters \code{p, N}.
#'
#' @param n The finitization order. It should be an integer > 1.
#' @param p The parameter of the finitized Binomial distribution - the success probability for each trial.\eqn{p \in [0,1]}
#' @param N The number of trials.\code{ n = 0, 1, 2, ...}
#' @param no The number of random values to be generated.
#'
#' @return \code{rpois} returns a vector of type \code{\link[base]{integer}} containing random values generated according to the finitized Binomial distribution.
#' The number of values is given by the parameter \code{no}.
#'
#' @examples
#' library(finitization)
#' rbinom(2, 0.5, 2, 10)
#'
#' @include utils.R
#' @export
rbinom <- function(n, p, N, no) {
    if(missing(n)) {
        message("argument n is missing!\n")
        return(invisible(NULL))
    }
    if(missing(N)) {
        message("argument N is missing!\n")
        return(invisible(NULL))
    }
    if(missing(p)) {
        message("argument p is missing!\n")
        return(invisible(NULL))
    }
    if(missing(no)) {
        message("argument no is missing!\n")
        return(invisible(NULL))
    }

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











