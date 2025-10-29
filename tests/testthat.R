if (requireNamespace("testthat", quietly = TRUE)) {
    library(testthat)
    library(finitization)
    test_check("finitization")
} else {
    message("Skipping tests: 'testthat' not installed in this build environment.")
}
