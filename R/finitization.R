#' finitization: A package that implements the finitization of probability distribution functions.
#'
#' This package contains functions that implement the concept of finitization of probability distribution functions.
#' It includes finitized versions of the Poisson, Logarithmic, Binomial, and Negative Binomial distributions.
#'
#' @name finitization
#' @keywords internal
#' @useDynLib finitization
#' @importFrom Rcpp evalCpp
"_PACKAGE"

# Called automatically when the package is loaded
.onLoad <- function(libname, pkgname) {
    try({
        initialize_finitization()
    }, silent = TRUE)
}
