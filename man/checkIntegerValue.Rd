% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{checkIntegerValue}
\alias{checkIntegerValue}
\title{Checks if a parameter has an integer value.}
\usage{
checkIntegerValue(no)
}
\arguments{
\item{no}{the parameter to be checked for validity.}
}
\value{
TRUE if \code{length(no) == 1} and \code{no} is an integer greater than 0, FALSE otherwise.
}
\description{
Checks if the parameter \code{no} satisfies \code{length(N) == 1} (no vectors with more than one element are allowed),
and if it is an integer greater than 0. If \code{no} is not an integer number, it will be converted to an integer value.
If the value passed through this parameter does not meet these criteria, the function returns FALSE, otherwise it
returns TRUE.
}
\keyword{internal}
