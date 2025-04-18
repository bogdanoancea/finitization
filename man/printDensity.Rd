% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{printDensity}
\alias{printDensity}
\title{Prints and returns the string representation of the density function.}
\usage{
printDensity(n, val, params, type, latex)
}
\arguments{
\item{n}{The finitization order.}

\item{val}{The value of the variable for which the probability density function is printed. If NULL, this function computes the
string representation of the pdf for all possible values, i.e. \code{{0 .. n}}.}

\item{params}{The parameters of the finitized distribution. \code{params} is a named list containig the parameters needed for a specific distribution:
the name of an item is the name of the parameters, and the value of an item is the value of the corresponding parameter.}

\item{type}{The distribution type. It could have one of the values returned by:
\itemize{
\item getPoissonType()
\item getBinomialType()
\item getNegativeBinomialType()
\item getLogarithmicType()
}}

\item{latex}{if TRUE, a Latex string of the pdf is printed and returned, otherwise the string has the format of an R expression.}
}
\value{
the string representation of the finitized probability distribution function.
}
\description{
Prints and returns the density function for finitized probability distributions.
}
\keyword{internal}
