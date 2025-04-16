test_that("rnegbinom returns a vector of correct length and valid outcomes", {
    set.seed(123)
    # For n = 2, the support is assumed to be 0, 1, and 2.
    res <- rnegbinom(n = 2, q = 0.15, k = 4, no = 1001)

    # Check that the output has the expected length.
    expect_length(res, 1001)

    # Check that the output is numeric.
    expect_true(is.numeric(res))

    # Check that each outcome is an integer (or equivalent to an integer).
    expect_true(all(res == floor(res)))

    # For n = 2, valid outcomes are 0, 1, or 2.
    expect_true(all(res %in% 0:2))
})

test_that("rnegbinom sample mean approximates expected mean", {
    set.seed(456)
    sample_size <- 1e6
    res <- rnegbinom(n = 2, q = 0.15, k = 4, no = sample_size)
    sample_mean <- mean(res)

    # Expected mean is approximately 0.7.
    expect_equal(sample_mean, 0.7, tolerance = 0.01)
})

test_that("rnegbinom sample variance approximates expected variance", {
    set.seed(789)
    sample_size <- 1e6
    res <- rnegbinom(n = 2, q = 0.15, k = 4, no = sample_size)
    sample_var <- var(res)

    # Expected variance is approximately 0.83.
    expect_equal(sample_var, 0.83, tolerance = 0.02)
})
