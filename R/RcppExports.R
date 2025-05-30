# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

c_printDensity <- function(n, val, params, dtype, latex = FALSE) {
    .Call(`_finitization_c_printDensity`, n, val, params, dtype, latex)
}

c_d <- function(n, val, params, dtype) {
    .Call(`_finitization_c_d`, n, val, params, dtype)
}

rvalues <- function(n, params, no, dtype) {
    .Call(`_finitization_rvalues`, n, params, no, dtype)
}

MFPS_pdf <- function(n, params, dtype) {
    .Call(`_finitization_MFPS_pdf`, n, params, dtype)
}

getPoissonType <- function() {
    .Call(`_finitization_getPoissonType`)
}

getNegativeBinomialType <- function() {
    .Call(`_finitization_getNegativeBinomialType`)
}

getBinomialType <- function() {
    .Call(`_finitization_getBinomialType`)
}

getLogarithmicType <- function() {
    .Call(`_finitization_getLogarithmicType`)
}

check_symbolic_equivalence <- function(expr1_str, expr2_str) {
    .Call(`_finitization_check_symbolic_equivalence`, expr1_str, expr2_str)
}

