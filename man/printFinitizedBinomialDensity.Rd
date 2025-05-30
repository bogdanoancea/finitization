% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binom.R
\name{printFinitizedBinomialDensity}
\alias{printFinitizedBinomialDensity}
\title{The string representation of the probability density function for the finitized Binomial distribution.}
\usage{
printFinitizedBinomialDensity(n, N, val = NULL, latex = FALSE)
}
\arguments{
\item{n}{The finitization order. An integer > 0.}

\item{N}{The number of trials.}

\item{val}{The value(s) at which the density is printed. If \code{NULL}, prints for all values from 0 to n.}

\item{latex}{Logical; if TRUE, the output is formatted in LaTeX.}
}
\value{
Silently returns a character vector with the string representation(s) of the pdf.
}
\description{
\code{printFinitizedBinomialDensity(n, N, val, latex)} computes and prints the string
representation of the probability density function for the finitized Binomial distribution.
}
\examples{
library(finitization)
printFinitizedBinomialDensity(4, 4)

}
