#' The density for the finitized Poisson distribution.
#'
#' \code{dpois(n, theta, val, log)} computes the finitized Poisson density for each value in \code{val}.
#'
#' @param n The finitization order. It should be an integer > 0.
#' @param theta The parameter of the finitized Poisson distribution.
#' @param val A vector with the values of the variable for which the probability density is computed.
#'            If \code{NULL}, a data frame containing all possible values (0, 1, ..., n)
#'            and the corresponding probabilities is returned.
#' @param log Logical; if TRUE, the (natural) logarithm of the computed probabilities is returned.
#'
#' @return A \code{data.frame} object with two columns: \code{val} containing the values and
#'         \code{prob} containing the corresponding densities (or their logarithms if \code{log = TRUE}).
#'
#' @examples
#' library(finitization)
#' dpois(4, 0.5, c(0,1,3))
#' dpois(4, 0.5, c(0,1,3), log = TRUE)
#'
#' @include utils.R
#' @export
dpois <- function(n, theta, val = NULL, log = FALSE) {
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

    d <- c_d(n, lim, list("theta" = theta), getPoissonType())
    if(any(d < 0) || any(d > 1))
        warning(paste0("Be sure that you provided parameter ", theta, " inside the maximum feasible parameter space"))

    # Return log probabilities if log = TRUE.
    if (log) {
        d <- log(d)
    }

    df <- data.frame(val = lim, prob = d)
    return(df)
}


#' The string representation of the probability density function for the finitized Poisson distribution.
#'
#' \code{printFinitizedPoissonDensity(n, val, latex)} computes and prints the string
#' representation of the probability density function for the finitized Poisson distribution.
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
#' printFinitizedPoissonDensity(4)
#'
#' @include utils.R
#' @export
printFinitizedPoissonDensity <-
    function(n, val = NULL, latex = FALSE)  {
        if(missing(n)) {
            message("Argument n is missing!\n")
            return(invisible(NULL))
        }
        if (!checkIntegerValue(n))
            return(invisible(NULL))
        if (!is.null(val) && !checkVals(n, val))
            return(invisible(NULL))

        r <- printDensity(n, val, NULL, getPoissonType(), latex)
        return(invisible(r))
    }

#' Maximum feasible parameter space  for the finitized Poisson distribution.
#'
#' \code{getPoissonMFPS(n)} computes and returns the maximum feasible parameter space for the finitized Poisson distribution.
#'
#' @param  n The finitization order. It should be an integer > 0.
#' @return A vector with two elements where the first element is the lower limit of the maximum feasible parameter space
#' and the second is the upper limit.
#'
#' @examples
#' library(finitization)
#' getPoissonMFPS(4)
#'
#' @include utils.R
#' @export
getPoissonMFPS <- function(n) {
    if(missing(n)) {
        message("Argument n is missing!\n")
        return(invisible(NULL))
    }
    if(!checkIntegerValue(n))
        return(invisible(NULL))

    fg <- function(theta) {
        "x"
    }
    body(fg)[[2]] <- parse(text = MFPS_pdf(n, NULL, getPoissonType()))[[1]]

    return(findSolutions(fg))
}


#' Random values generation  for the finitized Poisson distribution.
#'
#' \code{rpois(n, theta, no)} generates random values according to the finitized Poisson distribution with parameter \code{theta}.
#'
#' @param n The finitization order. It should be an integer > 1.
#' @param theta The parameter of the Poisson distribution.
#' @param no The number of random values to be generated.
#'
#' @return \code{rpois} returns a vector of type \code{\link[base]{integer}} containing random values generated according to the finitized Poisson distribution.
#' The number of values is given by the parameter \code{no}.
#'
#' @examples
#' library(finitization)
#' rpois(2, 0.5, 10)
#'
#' @include utils.R
#' @export
rpois <- function(n, theta, no) {
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

        return(rvalues(n, list("theta" = theta), no, getPoissonType()))
}

#' The cumulative distribution function (CDF) for the finitized Poisson distribution.
#'
#' \code{ppois(n, theta, val, log.p, lower.tail)} computes the CDF for the finitized Poisson distribution at the given value(s).
#'
#' @param n The finitization order. An integer > 0.
#' @param theta The parameter of the finitized Poisson distribution.
#' @param val A vector of values at which the CDF is computed. If \code{NULL},
#'            a data frame containing all possible values (0, 1, ..., n) and their cumulative probabilities is returned.
#' @param log.p Logical; if TRUE, the cumulative probabilities are returned on the logarithmic scale.
#' @param lower.tail Logical; if TRUE (default) probabilities are calculated as \eqn{P(X \le x)}; if FALSE, as \eqn{P(X > x)}.
#'
#' @return If \code{val} is provided, a numeric vector of cumulative probabilities (or their logarithms if \code{log.p = TRUE})
#'         is returned. Otherwise, a \code{data.frame} with columns \code{val} and \code{cdf} is returned.
#'
#' @examples
#' # For a finitized Poisson distribution with n = 4 and theta = 0.5:
#' ppois(n = 4, theta = 0.5, val = c(0, 2, 4))
#' ppois(n = 4, theta = 0.5, log.p = TRUE)
#' ppois(n = 4, theta = 0.5, val = c(0, 2, 4), lower.tail = FALSE)
#'
#' @include utils.R
#' @export
ppois <- function(n, theta, val = NULL, log.p = FALSE, lower.tail = TRUE) {
    if(missing(n)) {
        message("Argument 'n' is missing!\n")
        return(invisible(NULL))
    }
    if(missing(theta)) {
        message("Argument 'theta' is missing!\n")
        return(invisible(NULL))
    }

    if (!checkIntegerValue(n))
        return(invisible(NULL))
    if (!checkTheta(theta))
        return(invisible(NULL))

    # Compute the density for all outcomes 0, 1, ..., n.
    dens_all <- c_d(n, seq(0, n), list("theta" = theta), getPoissonType())
    cum_probs <- cumsum(dens_all)

    # If lower.tail is FALSE, convert to upper-tail probabilities: P(X > x) = 1 - P(X <= x).
    if (!lower.tail) {
        cum_probs <- 1 - cum_probs
    }

    if (!is.null(val)) {
        if (!checkVals(n, val))
            return(invisible(NULL))
        # Adjust for R's 1-indexing (outcomes are 0-indexed).
        cdf_vals <- cum_probs[val + 1]
        if (log.p)
            cdf_vals <- log(cdf_vals)
        return(cdf_vals)
    } else {
        df <- data.frame(val = seq(0, n), cdf = cum_probs)
        if (log.p)
            df$cdf <- log(df$cdf)
        return(df)
    }
}

#' The quantile function for the finitized Poisson distribution.
#'
#' \code{qpois(n, theta, p, lower.tail, log.p)} computes the quantile(s) corresponding to the given probability(ies)
#' for the finitized Poisson distribution.
#'
#' @param n The finitization order. An integer > 0.
#' @param theta The parameter of the finitized Poisson distribution.
#' @param p A numeric vector of probabilities. For \code{lower.tail = TRUE} (default), these are interpreted as \eqn{P(X \le x)};
#'          if \code{lower.tail = FALSE} they are interpreted as \eqn{P(X > x)}.
#' @param lower.tail Logical; if TRUE (default) the input probabilities are lower-tail probabilities; otherwise, they are upper-tail.
#' @param log.p Logical; if TRUE the probabilities in \code{p} are assumed to be on the logarithmic scale.
#'
#' @return A numeric vector of quantiles corresponding to the input probabilities. Each quantile is defined as the
#'         smallest integer \eqn{x} for which the cumulative probability is at least the given probability.
#'
#' @examples
#' # For a finitized Poisson distribution with n = 4 and theta = 0.5:
#' qpois(n = 4, theta = 0.5, p = c(0.1, 0.5, 0.9))
#' qpois(n = 4, theta = 0.5, p = c(0.1, 0.5, 0.9), lower.tail = FALSE)
#'
#' @include utils.R
#' @export
qpois <- function(n, theta, p, lower.tail = TRUE, log.p = FALSE) {
    if(missing(n)) {
        message("Argument 'n' is missing!\n")
        return(invisible(NULL))
    }
    if(missing(theta)) {
        message("Argument 'theta' is missing!\n")
        return(invisible(NULL))
    }
    if(missing(p)) {
        message("Argument 'p' is missing!\n")
        return(invisible(NULL))
    }

    if (!checkIntegerValue(n))
        return(invisible(NULL))
    if (!checkTheta(theta))
        return(invisible(NULL))
    if (!is.numeric(p))
        stop("Argument 'p' must be numeric.")

    # Convert log probabilities to the standard scale if needed.
    if (log.p) {
        p <- exp(p)
    }

    # For lower.tail = FALSE, convert upper-tail probabilities to lower-tail equivalents.
    if (!lower.tail) {
        p <- 1 - p
    }

    if (any(p < 0 | p > 1)) {
        stop("Probabilities in 'p' must be between 0 and 1.")
    }

    # Compute the density for all outcomes 0, 1, ..., n.
    supp <- seq(0, n)
    dens <- c_d(n, supp, list("theta" = theta), getPoissonType())
    cum_probs <- cumsum(dens)

    # For each probability, find the smallest outcome for which the CDF is at least that probability.
    quantiles <- sapply(p, function(prob) {
        ix <- which(cum_probs >= prob)[1]
        if (is.na(ix)) NA else supp[ix]
    })
    return(quantiles)
}
