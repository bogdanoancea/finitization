% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{checkTheta}
\alias{checkTheta}
\title{Checks the validity of the parameter \code{theta} of the finitized Poisson and/or Logarithmic distribution.}
\usage{
checkTheta(theta)
}
\arguments{
\item{theta}{The parameter of the finitized Poisson distribution.}
}
\value{
TRUE if \code{length(theta) == 1} and \code{theta} is a double in\code{[0,1]} interval, FALSE otherwise.
}
\description{
Checks if the parameter of the finitized Poisson distribution satisfies \code{length(theta) == 1} (no vectors with more than one element are allowed),
and if it is a double with values in \code{[0,1]}. If the value passed through this parameter does not meet these criteria, the function returns FALSE, otherwise it
returns TRUE.
}
\keyword{internal}
