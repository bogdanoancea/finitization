#' The density for the finitized Negative Binomial distribution.
#'
#' \code{dnegbinom(n, q, k, val, log)} computes the finitized Negative Binomial density for \code{val}.
#'
#' @param n The finitization order. It should be an integer > 0.
#' @param q The parameter of the finitized Negative Binomial distribution - the success probability for each trial (\eqn{q \in [0,1]}).
#' @param k The number of failures until the experiment is stopped, \code{k > 0}.
#' @param val A vector with the values of the variable for which the probability density is computed. If \code{NULL},
#'            a data frame containing all possible values, i.e. \code{0 ... n}, and the corresponding probabilities is returned.
#' @param log Logical; if TRUE, the (natural) logarithm of the computed densities is returned.
#'
#' @return A \code{data.frame} object with two columns: \code{val} containing the values and \code{prob} containing the corresponding densities (or their logarithms if \code{log = TRUE}).
#'
#' @examples
#' library(finitization)
#' dnegbinom(4, 0.12, 4)
#' dnegbinom(4, 0.12, 4, log = TRUE)
#'
#' @include utils.R
#' @export
dnegbinom <- function(n, q, k, val = NULL, log = FALSE) {
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

    # Return log densities if requested.
    if (log) {
        d <- log(d)
    }

    df <- data.frame(val = lim, prob = d)
    return(df)
}

#' The quantile function for the finitized Negative Binomial distribution.
#'
#' \code{qnegbinomial(n, q, k, p, lower.tail = TRUE, log.p = FALSE)} computes the quantile(s)
#' corresponding to the given probability(ies) for the finitized Negative Binomial distribution.
#'
#' @param n The finitization order. An integer > 0.
#' @param q The success probability for each trial (\eqn{q \in [0,1]}).
#' @param k The number of failures until the experiment is stopped, \code{k > 0}.
#' @param p A numeric vector of probabilities. For \code{lower.tail = TRUE} (default), these are interpreted
#' as \eqn{P(X \le x)}; if \code{lower.tail = FALSE}, they represent \eqn{P(X > x)}.
#' @param lower.tail Logical; if TRUE (default) the input probabilities are lower-tail probabilities,
#' otherwise they are upper-tail probabilities.
#' @param log.p Logical; if TRUE, the probabilities in \code{p} are assumed to be on the logarithmic scale.
#'
#' @return A numeric vector of quantiles corresponding to the input probabilities. Each quantile is the smallest integer
#' for which the cumulative probability is at least the given probability.
#'
#' @examples
#' library(finitization)
#' qnegbinomial(n = 4, q = 0.12, k = 4, p = c(0.1, 0.5, 0.9))
#' qnegbinomial(n = 4, q = 0.12, k = 4, p = c(0.1, 0.5, 0.9), lower.tail = FALSE)
#'
#' @include utils.R
#' @export
qnegbinom <- function(n, q, k, p, lower.tail = TRUE, log.p = FALSE) {
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
    if(missing(p)) {
        message("Argument p is missing!\n")
        return(invisible(NULL))
    }

    if(!checkIntegerValue(n))
        return(invisible(NULL))
    if(!checkNegBinomialQ(q))
        return(invisible(NULL))
    if(!checkIntegerValue(k))
        return(invisible(NULL))
    if(!is.numeric(p))
        stop("Argument 'p' must be numeric.")

    # Convert from log probabilities if necessary.
    if(log.p) {
        p <- exp(p)
    }
    # Convert upper-tail to lower-tail if needed.
    if (!lower.tail) {
        p <- 1 - p
    }

    if(any(p < 0 | p > 1)) {
        stop("Probabilities in 'p' must be between 0 and 1.")
    }

    # Define the finite support: outcomes 0, 1, ..., n.
    supp <- seq(0, n)
    # Compute the density over the support.
    dens <- c_d(n, supp, list("q" = q, "k" = k), getNegativeBinomialType())
    # Compute the cumulative distribution function (CDF).
    cdf <- cumsum(dens)

    # For each input probability, if prob equals 1, return the maximum outcome.
    # Otherwise, find the smallest outcome for which the CDF is at least that probability.
    quantiles <- sapply(p, function(prob) {
        if(prob == 1) {
            return(supp[length(supp)])
        }
        ix <- which(cdf >= prob)[1]
        if(is.na(ix)) NA else supp[ix]
    })

    return(quantiles)
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

#' The cumulative distribution function (CDF) for the finitized Negative Binomial distribution.
#'
#' \code{pnegbinom(n, q, k, val, log.p, lower.tail)} computes the CDF for the finitized Negative Binomial distribution at the given value(s).
#'
#' @param n The finitization order. An integer > 0.
#' @param q The success probability for each trial (\eqn{q \in [0,1]}).
#' @param k The number of failures until the experiment is stopped, \code{k > 0}.
#' @param val A vector with the values at which the CDF is computed. If \code{NULL},
#'            a data frame containing all possible values (0, 1, ..., n) and their cumulative probabilities is returned.
#' @param log.p Logical; if TRUE, the cumulative probabilities are returned on the logarithmic scale.
#' @param lower.tail Logical; if TRUE (default) probabilities are calculated as \eqn{P(X \le x)}; if FALSE, as \eqn{P(X > x)}.
#'
#' @return If \code{val} is provided, a numeric vector of cumulative probabilities (or their logarithms if \code{log.p = TRUE}) is returned.
#'         Otherwise, a \code{data.frame} with columns \code{val} and \code{cdf} is returned.
#'
#' @examples
#' library(finitization)
#' pnegbinom(4, 0.12, 4, val = c(0, 2, 4))
#' pnegbinom(4, 0.12, 4, log.p = TRUE)
#' pnegbinom(4, 0.12, 4, val = c(0, 2, 4), lower.tail = FALSE)
#'
#' @include utils.R
#' @export
pnegbinom <- function(n, q, k, val = NULL, log.p = FALSE, lower.tail = TRUE) {
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

    # Compute the density for all outcomes 0, 1, ..., n.
    dens_all <- c_d(n, seq(0, n), list("q" = q, "k" = k), getNegativeBinomialType())
    cum_probs <- cumsum(dens_all)

    # If lower.tail is FALSE, compute the upper-tail probabilities.
    if (!lower.tail) {
        cum_probs <- 1 - cum_probs
    }

    if (!is.null(val)) {
        if (!checkVals(n, val))
            return(invisible(NULL))
        # Adjust for R's 1-indexing (since outcomes start at 0).
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

