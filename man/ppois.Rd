% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pois.R
\name{ppois}
\alias{ppois}
\title{The cumulative distribution function (CDF) for the finitized Poisson distribution.}
\usage{
ppois(n, theta, val = NULL, log.p = FALSE, lower.tail = TRUE)
}
\arguments{
\item{n}{The finitization order. A positive integer (> 0).}

\item{theta}{The mean parameter of the finitized Poisson distribution.}

\item{val}{A numeric vector of values at which to compute the CDF.
If \code{NULL} (default), the function returns the full CDF over the entire support (0, 1, ..., n).}

\item{log.p}{Logical; if \code{TRUE}, returns cumulative probabilities on the log scale.}

\item{lower.tail}{Logical; if \code{TRUE} (default), probabilities are calculated as \eqn{P(X \le x)}.
If \code{FALSE}, returns upper-tail probabilities \eqn{P(X > x)}.}
}
\value{
A \code{data.frame} with columns:
\describe{
\item{\code{val}}{The evaluated values (input \code{val} or the full range \code{0:n}).}
\item{\code{cdf}}{The corresponding cumulative probabilities (or log-probabilities if \code{log.p = TRUE}).}
}
}
\description{
\code{ppois(n, theta, val, log.p, lower.tail)} computes the CDF for the finitized Poisson distribution at specified value(s).
}
\examples{
# Compute CDF at specific values
ppois(n = 4, theta = 0.5, val = c(0, 2, 4))

# Compute full CDF over range 0 to 4
ppois(n = 4, theta = 0.5)

# Upper-tail probabilities
ppois(n = 4, theta = 0.5, val = c(0, 2, 4), lower.tail = FALSE)

# Log-scale cumulative probabilities
ppois(n = 4, theta = 0.5, log.p = TRUE)

}
