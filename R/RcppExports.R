# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#' Generates a string representation of the probability density function.
#'
#' @param n The finitization order. It should be an integer > 0.
#' @param val The value of the variable for which the probability density function is computed.
#' @param params Other parameters of the distribution. They are provided in a list with named items, where the name
#' of an item is the name of the parameter.
#' @param dtype The type of the distribution: Poisson, Binomial, NegativeBinomial, Logaritmic.
#' @param latex If true it returns a Latex formatted string representation of the pdf, otherwise it returns
#' the string representation of the pdf as an R expression.
#' @keywords internal
#' @return a string representation of the pdf.
c_printDensity <- function(n, val, params, dtype, latex = FALSE) {
    .Call('_finitization_c_printDensity', PACKAGE = 'finitization', n, val, params, dtype, latex)
}

#' Computes the probability density of a finitized distribution.
#'
#' @param n The finitization order. It should be an integer > 0.
#' @param val The value of the variable for which the probability density function is computed.
#' @param params Other parameters of the distribution. They are provided in a list with named items, where the name
#' of an item is the name of the parameter.
#' @param dtype The type of the distribution: Poisson, Binomial, NegativeBinomial, Logaritmic.
#' @keywords internal
#' @return a\code{NumericVector} with the values of the density for each value provide in \code{val}.
c_d <- function(n, val, params, dtype) {
    .Call('_finitization_c_d', PACKAGE = 'finitization', n, val, params, dtype)
}

#' Random values generation.
#'
#' @param n The finitization order. It should be an integer > 0.
#' @param params Other parameters of the distribution. They are provided in a list with named items, where the name
#' of an item is the name of the parameter.
#' @param dtype The type of the distribution: Poisson, Binomial, NegativeBinomial, Logaritmic.
#' @param no The number of random values to be generated.
#' @keywords internal
#' @return a\code{NumericVector} with the random values generated.
rvalues <- function(n, params, no, dtype) {
    .Call('_finitization_rvalues', PACKAGE = 'finitization', n, params, no, dtype)
}

#' Computes the expression of the \code{pdf(n-1)} needed to compute the maximu feasible parameter space.
#'
#' @param n The finitization order. It should be an integer > 0.
#' @param params Other parameters of the distribution. They are provided in a list with named items, where the name
#' of an item is the name of the parameter.
#' @param dtype The type of the distribution: Poisson, Binomial, NegativeBinomial, Logaritmic.
#' @keywords internal
#' @return the expression of the \code{pdf(n-1)}.
MFPS_pdf <- function(n, params, dtype) {
    .Call('_finitization_MFPS_pdf', PACKAGE = 'finitization', n, params, dtype)
}

#' The Poisson distribution type.
#'
#' @return the Poisson distribution type.
#' @keywords internal
getPoissonType <- function() {
    .Call('_finitization_getPoissonType', PACKAGE = 'finitization')
}

#' The Negative Binomial distribution type.
#'
#' @return the Negative Binomial distribution type.
#' @keywords internal
getNegativeBinomialType <- function() {
    .Call('_finitization_getNegativeBinomialType', PACKAGE = 'finitization')
}

#' The Binomial distribution type.
#'
#' @return the Binomial distribution type.
#' @keywords internal
getBinomialType <- function() {
    .Call('_finitization_getBinomialType', PACKAGE = 'finitization')
}

#' The Logarithmic distribution type.
#'
#' @return the Logarithmic distribution type.
#' @keywords internal
getLogarithmicType <- function() {
    .Call('_finitization_getLogarithmicType', PACKAGE = 'finitization')
}

