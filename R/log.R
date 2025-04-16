#' The density for the Logarithmic distribution.
#'
#' \code{dlog(n, theta, val, log)} computes the finitized Logarithmic density for each value in \code{val}.
#'
#' @param n The finitization order. It should be an integer > 0.
#' @param theta The parameter of the finitized Logarithmic distribution.
#' @param val A vector with the values of the variable for which the probability density is computed.
#'            If \code{NULL}, a data frame containing all possible values, i.e. \code{0 ... n}, and the corresponding
#'            probabilities is returned.
#' @param log Logical; if TRUE, the (natural) logarithm of the computed probabilities is returned.
#'
#' @return A \code{data.frame} object with two columns: \code{val} containing the values and
#'         \code{prob} containing the corresponding densities (or their logarithms if \code{log = TRUE}).
#'
#' @examples
#' library(finitization)
#' dlog(4, 0.1, c(0,1,3))
#' dlog(4, 0.1, c(0,1,3), log = TRUE)
#'
#' @include utils.R
#' @export
dlog <- function(n, theta, val = NULL, log = FALSE) {
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

    # Return log densities if requested.
    if (log) {
        d <- log(d)
    }

    df <- data.frame(val = lim, prob = d)
    return(df)
}

#' Maximum feasible parameter space for the finitized Logarithmic distribution.
#'
#' \code{getLogarithmicMFPS(n)} computes and returns the maximum feasible parameter space for the finitized Logarithmic distribution.
#'
#' @param n The finitization order. It should be an integer > 0.
#'
#' @return A vector with two elements where the first element is the lower limit and the second is the upper limit.
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
#' \code{printFinitizedLogarithmicDensity(n, val, latex)} computes and prints the string representation of the pdf.
#'
#' @param n The finitization order. It should be an integer > 0.
#' @param val The value for which the pdf is printed. If \code{NULL}, the pdf for all values (0 ... n) is printed.
#' @param latex Logical; if TRUE the output is formatted in LaTeX.
#'
#' @return A character vector containing the string representation(s) of the pdf.
#'
#' @examples
#' library(finitization)
#' printFinitizedLogarithmicDensity(4)
#'
#' @include utils.R
#' @export
printFinitizedLogarithmicDensity <- function(n, val = NULL, latex = FALSE) {
    if (!checkIntegerValue(n))
        return(invisible(NULL))
    if (!is.null(val) && !checkVals(n, val))
        return(invisible(NULL))

    r <- printDensity(n, val, NULL, getLogarithmicType(), latex)
    return(invisible(r))
}

#' Random values generation for the finitized Logarithmic distribution.
#'
#' \code{rlog(n, theta, no)} generates random values according to the finitized Logarithmic distribution.
#'
#' @param n The finitization order. It should be an integer > 1.
#' @param theta The parameter of the Logarithmic distribution.
#' @param no The number of random values to be generated.
#'
#' @return A vector of integers containing random values generated from the finitized Logarithmic distribution.
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

#' The cumulative distribution function (CDF) for the finitized Logarithmic distribution.
#'
#' \code{plog(n, theta, val, log.p, lower.tail)} computes the CDF for the finitized Logarithmic distribution at the given value(s).
#'
#' @param n The finitization order. An integer > 0.
#' @param theta The parameter of the finitized Logarithmic distribution.
#' @param val A vector with the values at which the CDF is computed. If \code{NULL},
#'            a data frame containing all possible values (0, 1, ..., n) and their cumulative probabilities is returned.
#' @param log.p Logical; if TRUE, the cumulative probabilities are returned on the logarithmic scale.
#' @param lower.tail Logical; if TRUE (default) probabilities are computed as \eqn{P(X \le x)}; if FALSE, as \eqn{P(X > x)}.
#'
#' @return If \code{val} is provided, a numeric vector of cumulative probabilities (or their logarithms if \code{log.p = TRUE}) is returned.
#'         Otherwise, a \code{data.frame} with columns \code{val} and \code{cdf} is returned.
#'
#' @examples
#' library(finitization)
#' plog(4, 0.1, val = c(0, 2, 4))
#' plog(4, 0.1, log.p = TRUE)
#' plog(4, 0.1, val = c(0, 2, 4), lower.tail = FALSE)
#'
#' @export
plog <- function(n, theta, val = NULL, log.p = FALSE, lower.tail = TRUE) {
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
    dens_all <- c_d(n, seq(0, n), list("theta" = theta), getLogarithmicType())
    cum_probs <- cumsum(dens_all)

    # If lower.tail is FALSE, convert to upper-tail probabilities: P(X > x) = 1 - P(X <= x).
    if (!lower.tail) {
        cum_probs <- 1 - cum_probs
    }

    if (!is.null(val)) {
        if (!checkVals(n, val))
            return(invisible(NULL))
        # Adjust for R's 1-indexing since outcomes start at 0.
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

#' The quantile function for the finitized Logarithmic distribution.
#'
#' \code{qlog(n, theta, p, lower.tail, log.p)} computes the quantile function for the finitized Logarithmic distribution.
#'
#' @param n The finitization order. An integer > 0.
#' @param theta The parameter of the finitized Logarithmic distribution.
#' @param p A numeric vector of probabilities. For \code{lower.tail = TRUE} (default), these are interpreted as \eqn{P(X \le x)};
#'          if \code{lower.tail = FALSE} they are interpreted as \eqn{P(X > x)}.
#' @param lower.tail Logical; if TRUE (default) the input probabilities are lower-tail probabilities, otherwise upper-tail probabilities.
#' @param log.p Logical; if TRUE, the probabilities in \code{p} are assumed to be given on the logarithmic scale.
#'
#' @return A numeric vector of quantiles corresponding to the input probabilities.
#'
#' @examples
#' library(finitization)
#' qlog(4, 0.1, p = c(0.1, 0.1, 0.9))
#' qlog(4, 0.1, p = c(0.1, 0.1, 0.9), lower.tail = FALSE)
#'
#' @export
qlog <- function(n, theta, p, lower.tail = TRUE, log.p = FALSE) {
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
    # If upper-tail probabilities are provided, convert them to lower-tail.
    if (!lower.tail) {
        p <- 1 - p
    }
    if (any(p < 0 | p > 1)) {
        stop("Probabilities in 'p' must be between 0 and 1.")
    }

    # Compute the density for all outcomes 0, 1, ..., n.
    supp <- seq(0, n)
    dens <- c_d(n, supp, list("theta" = theta), getLogarithmicType())
    cum_probs <- cumsum(dens)

    # For each probability value, find the smallest outcome for which the CDF is at least that probability.
    quantiles <- sapply(p, function(prob) {
        ix <- which(cum_probs >= prob)[1]
        if (is.na(ix))
            return(NA)
        else
            return(supp[ix])
    })
    return(quantiles)
}
