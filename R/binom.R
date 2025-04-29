#' The density for the finitized Binomial distribution.
#'
#' \code{dbinom(n, p, N, val, log)} computes the finitized Binomial density for each value in \code{val}.
#'
#' @param n The finitization order. An integer > 0.
#' @param p The success probability for each trial (must satisfy 0 <= p <= 1).
#' @param N The number of trials.
#' @param val A vector of values at which the density is computed. If \code{NULL},
#'            a data frame containing all possible values (from 0 to n) and the corresponding densities is returned.
#' @param log Logical; if TRUE, the (natural) logarithm of the probabilities is returned.
#'
#' @return A \code{data.frame} with two columns: \code{val}, which contains the values, and \code{prob},
#'         which contains the corresponding density (or log density if \code{log = TRUE}).
#'
#' @examples
#' library(finitization)
#' dbinom(4, 0.5, 4)
#' dbinom(4, 0.5, 4, log = TRUE)
#'
#' @include utils.R
#' @export
dbinom <- function(n, p, N, val = NULL, log = FALSE) {
    if(missing(n)) {
        message("Argument n is missing!\n")
        return(invisible(NULL))
    }
    if(missing(p)) {
        message("Argument p is missing!\n")
        return(invisible(NULL))
    }
    if(missing(N)) {
        message("Argument N is missing!\n")
        return(invisible(NULL))
    }

    if (!checkIntegerValue(n))
        return(invisible(NULL))
    if (!checkBinomialP(p))
        return(invisible(NULL))
    if (!checkIntegerValue(N))
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
        warning("Be sure that you provided parameters inside the maximum feasible parameter space")

    # Return log densities if requested
    if (log) {
        d <- log(d)
    }

    df <- data.frame(val = lim, prob = d)
    return(df)
}

#' The string representation of the probability density function for the finitized Binomial distribution.
#'
#' \code{printFinitizedBinomialDensity(n, N, val, latex)} computes and prints the string
#' representation of the probability density function for the finitized Binomial distribution.
#'
#' @param n The finitization order. An integer > 0.
#' @param N The number of trials.
#' @param val The value(s) at which the density is printed. If \code{NULL}, prints for all values from 0 to n.
#' @param latex Logical; if TRUE, the output is formatted in LaTeX.
#'
#' @return Silently returns a character vector with the string representation(s) of the pdf.
#'
#' @examples
#' library(finitization)
#' printFinitizedBinomialDensity(4, 4)
#'
#' @include utils.R
#' @export
printFinitizedBinomialDensity <- function(n, N, val = NULL, latex = FALSE)  {
    if(missing(n)) {
        message("Argument n is missing!\n")
        return(invisible(NULL))
    }
    if(missing(N)) {
        message("Argument N is missing!\n")
        return(invisible(NULL))
    }
    if (!checkIntegerValue(n))
        return(invisible(NULL))
    if (!checkIntegerValue(N))
        return(invisible(NULL))
    if (!is.null(val) && !checkVals(n, val))
        return(invisible(NULL))

    r <- printDensity(n, val, list("N" = N), getBinomialType(), latex)

    return(invisible(r))
}

#' Maximum feasible parameter space for the finitized Binomial distribution.
#'
#' \code{getBinomialMFPS(n, N)} computes and returns the maximum feasible parameter space for the finitized Binomial distribution.
#'
#' @param n The finitization order. An integer > 0.
#' @param N The number of trials.
#'
#' @return A vector of two elements, where the first element is the lower limit and the second element is the upper limit of the maximum feasible parameter space.
#'
#' @examples
#' library(finitization)
#' getBinomialMFPS(2, 4)
#'
#' @include utils.R
#' @export
getBinomialMFPS <- function(n, N) {
    if(missing(n)) {
        message("Argument n is missing!\n")
        return(invisible(NULL))
    }
    if(missing(N)) {
        message("Argument N is missing!\n")
        return(invisible(NULL))
    }

    if(!checkIntegerValue(n))
        return(invisible(NULL))
    if (!checkIntegerValue(N))
        return(invisible(NULL))

    fg <- function(p) {
        "x"
    }
    body(fg)[[2]] <- parse(text = MFPS_pdf(n, list("N" = N), getBinomialType()))[[1]]

    return(findSolutions(fg))
}

#' Random values generation for the finitized Binomial distribution.
#'
#' \code{rbinom(n, p, N, no)} generates random values according to the finitized Binomial distribution.
#'
#' @param n The finitization order. An integer > 1.
#' @param p The success probability for each trial (0 <= p <= 1).
#' @param N The number of trials.
#' @param no The number of random values to be generated.
#'
#' @return An integer vector of length \code{no}, with random values drawn from the finitized Binomial distribution.
#'
#' @examples
#' library(finitization)
#' rbinom(2, 0.5, 2, 10)
#'
#' @include utils.R
#' @export
rbinom <- function(n, p, N, no) {
    if(missing(n)) {
        message("Argument n is missing!\n")
        return(invisible(NULL))
    }
    if(missing(N)) {
        message("Argument N is missing!\n")
        return(invisible(NULL))
    }
    if(missing(p)) {
        message("Argument p is missing!\n")
        return(invisible(NULL))
    }
    if(missing(no)) {
        message("Argument no is missing!\n")
        return(invisible(NULL))
    }

    if (!checkIntegerValue(n))
        return(invisible(NULL))
    if (!checkBinomialP(p))
        return(invisible(NULL))
    if (!checkIntegerValue(N))
        return(invisible(NULL))
    if (!checkIntegerValue(no))
        return(invisible(NULL))

    return(rvalues(n, list("p" = p, "N" = N), no, getBinomialType()))
}

#' The cumulative distribution function (CDF) for the finitized Binomial distribution.
#'
#' \code{pbinom(n, p, N, val, lower.tail, log.p)} computes the CDF for the finitized Binomial distribution at the given value(s).
#'
#' @param n The finitization order. An integer > 0.
#' @param p The success probability for each trial (0 <= p <= 1).
#' @param N The number of trials.
#' @param val A vector of values at which the CDF is computed. If \code{NULL},
#'            a data frame containing all possible values (from 0 to n) and the corresponding cumulative probabilities is returned.
#' @param lower.tail Logical; if TRUE (default) probabilities are computed as \eqn{P(X \le x)}; if FALSE, as \eqn{P(X > x)}.
#' @param log.p Logical; if TRUE, the cumulative probabilities are returned on the logarithmic scale.
#'
#' @return If \code{val} is provided, a numeric vector of cumulative probabilities (or their logarithms if \code{log.p = TRUE}) is returned.
#'         Otherwise, a \code{data.frame} with columns \code{val} and \code{cdf} is returned.
#'
#' @examples
#' library(finitization)
#' pbinom(4, 0.5, 4, val = c(0, 2, 4))
#' pbinom(4, 0.5, 4, lower.tail = FALSE)
#' pbinom(4, 0.5, 4, log.p = TRUE)
#'
#' @include utils.R
#' @export
pbinom <- function(n, p, N, val = NULL, lower.tail = TRUE, log.p = FALSE) {
    if(missing(n)) {
        message("Argument n is missing!\n")
        return(invisible(NULL))
    }
    if(missing(p)) {
        message("Argument p is missing!\n")
        return(invisible(NULL))
    }
    if(missing(N)) {
        message("Argument N is missing!\n")
        return(invisible(NULL))
    }

    if (!checkIntegerValue(n))
        return(invisible(NULL))
    if (!checkBinomialP(p))
        return(invisible(NULL))
    if (!checkIntegerValue(N))
        return(invisible(NULL))

    # Calculate the cumulative distribution using the density over all outcomes 0, 1, ..., n.
    dens_all <- c_d(n, seq(0, n), list("p" = p, "N" = N), getBinomialType())
    cum_probs <- cumsum(dens_all)

    # If lower.tail is FALSE, return upper-tail probabilities.
    if (!lower.tail) {
        cum_probs <- 1 - cum_probs
    }

    if (!is.null(val)) {
        # Validate the provided values.
        if (!checkVals(n, val))
            return(invisible(NULL))
        # Adjust for R's 1-indexing (since outcomes start at 0).
        cdf_vals <- cum_probs[val + 1]
        if (log.p)
            cdf_vals <- log(cdf_vals)
        df <- data.frame(val = val, cdf = cdf_vals)
        return(df)
    } else {
        df <- data.frame(val = seq(0, n), cdf = cum_probs)
        if (log.p)
            df$cdf <- log(df$cdf)
        return(df)
    }
}


#' The quantile function for the finitized Binomial distribution.
#'
#' \code{qbinom(n, p, N, prob, lower.tail = TRUE, log.p = FALSE)} computes the quantile function for the finitized Binomial distribution.
#'
#' @param n The finitization order. An integer > 0.
#' @param p The success probability for each trial (0 <= p <= 1).
#' @param N The number of trials.
#' @param prob A vector of probabilities (or log-probabilities if \code{log.p = TRUE}) for which the quantiles are computed.
#'             Each probability must be between 0 and 1.
#' @param lower.tail Logical; if TRUE (default) the input probabilities are interpreted as \eqn{P(X \le x)};
#'                   if FALSE, they are interpreted as \eqn{P(X > x)}.
#' @param log.p Logical; if TRUE, the probabilities in \code{prob} are assumed to be given on the logarithmic scale.
#'
#' @return A numeric vector of quantiles corresponding to the input probabilities. For each probability,
#'         the quantile is defined as the smallest integer \(x\) (from 0 to n) such that the cumulative probability
#'         is at least the provided value.
#'
#' @examples
#' library(finitization)
#' qbinom(4, 0.5, 4, prob = c(0.1, 0.5, 0.9))
#' qbinom(4, 0.5, 4, prob = log(c(0.1, 0.5, 0.9)), lower.tail = TRUE, log.p = TRUE)
#' qbinom(4, 0.5, 4, prob = c(0.1, 0.5, 0.9), lower.tail = FALSE)
#'
#' @include utils.R
#' @export
qbinom <- function(n, p, N, prob, lower.tail = TRUE, log.p = FALSE) {
    if(missing(n)) {
        message("Argument n is missing!\n")
        return(invisible(NULL))
    }
    if(missing(p)) {
        message("Argument p is missing!\n")
        return(invisible(NULL))
    }
    if(missing(N)) {
        message("Argument N is missing!\n")
        return(invisible(NULL))
    }
    if(missing(prob)) {
        message("Argument prob is missing!\n")
        return(invisible(NULL))
    }

    if (!checkIntegerValue(n))
        return(invisible(NULL))
    if (!checkBinomialP(p))
        return(invisible(NULL))
    if (!checkIntegerValue(N))
        return(invisible(NULL))

    # If probabilities are provided in log scale, convert them to the standard scale.
    if (log.p) {
        prob <- exp(prob)
    }

    # Adjust for upper-tail probabilities if needed.
    if (!lower.tail) {
        prob <- 1 - prob
    }

    if (any(prob < 0 | prob > 1)) {
        stop("Probabilities must be between 0 and 1")
    }

    # Compute the cumulative probabilities from the density over outcomes 0, 1, ..., n.
    dens_all <- c_d(n, seq(0, n), list("p" = p, "N" = N), getBinomialType())
    cum_probs <- cumsum(dens_all)

    # For each input probability, find the smallest outcome x for which the cumulative probability is at least that value.
    quantiles <- sapply(prob, function(q) {
        ix <- which(cum_probs >= q)[1]
        if (is.na(ix)) NA else ix - 1  # Adjust for 0-indexing.
    })

    return(quantiles)
}

