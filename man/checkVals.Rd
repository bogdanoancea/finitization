% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{checkVals}
\alias{checkVals}
\title{Checks if the values of the variable are valid.}
\usage{
checkVals(n, val)
}
\arguments{
\item{n}{The finitization order of the distribution}

\item{val}{a vector with the values of the variable.}
}
\value{
TRUE if all values in \code{val} are integers and they are in the set \code{{0, 1, 2, ... n}}, FALSE otherwise.
}
\description{
Checks if the values of the variable are valid, i.e. they belong to \code{{0, 1, 2, ... n}}, where \code{n} is the finitization order.
}
\keyword{internal}
