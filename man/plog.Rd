% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/log.R
\name{plog}
\alias{plog}
\title{The cumulative distribution function (CDF) for the finitized Logarithmic distribution.}
\usage{
plog(n, theta, val = NULL, log.p = FALSE, lower.tail = TRUE)
}
\arguments{
\item{n}{The finitization order. An integer > 0.}

\item{theta}{The parameter of the finitized Logarithmic distribution.}

\item{val}{A vector with the values at which the CDF is computed. If \code{NULL},
a data frame containing all possible values (0, 1, ..., n) and their cumulative probabilities is returned.}

\item{log.p}{Logical; if TRUE, the cumulative probabilities are returned on the logarithmic scale.}

\item{lower.tail}{Logical; if TRUE (default) probabilities are computed as \eqn{P(X \le x)}; if FALSE, as \eqn{P(X > x)}.}
}
\value{
If \code{val} is provided, a numeric vector of cumulative probabilities (or their logarithms if \code{log.p = TRUE}) is returned.
Otherwise, a \code{data.frame} with columns \code{val} and \code{cdf} is returned.
}
\description{
\code{plog(n, theta, val, log.p, lower.tail)} computes the CDF for the finitized Logarithmic distribution at the given value(s).
}
\examples{
library(finitization)
plog(4, 0.1, val = c(0, 2, 4))
plog(4, 0.1, log.p = TRUE)
plog(4, 0.1, val = c(0, 2, 4), lower.tail = FALSE)

}
