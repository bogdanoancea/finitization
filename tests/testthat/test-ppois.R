test_that("ppois returns a data frame with full support when val is NULL", {
    # Expected cumulative probabilities for n = 4, theta = 0.5.
    expected_cdf <- c(0.606770833, 0.908854166, 0.986979166, 0.997395833, 1.0)

    # Invoke ppois with no specified 'val' so that the full support 0:4 is returned.
    result <- ppois(n = 4, theta = 0.5, val = NULL, log.p = FALSE, lower.tail = TRUE)

    # Check that a data frame is returned.
    expect_s3_class(result, "data.frame")
    expect_true(all(c("val", "cdf") %in% names(result)))

    # Check that the support is 0:4 and the cumulative probabilities are as expected.
    expect_equal(result$val, 0:4)
    expect_equal(result$cdf, expected_cdf, tolerance = 1e-8)
})

test_that("ppois returns correct lower-tail probabilities for specified val", {
    # For n = 4, theta = 0.5 and val = c(0,2,4)
    expected <- c(0.606770833, 0.986979166, 1.0)

    result <- ppois(n = 4, theta = 0.5, val = c(0, 2, 4), log.p = FALSE, lower.tail = TRUE)
    expect_equal(result, expected, tolerance = 1e-8)
})

test_that("ppois returns correct upper-tail probabilities when lower.tail is FALSE", {
    # For n = 4, theta = 0.5 and val = c(0,2,4):
    # Upper-tail probabilities = 1 - lower-tail probability.
    # For 0: 1 - 0.606770833 = 0.393229167, 2: 1 - 0.986979166 = 0.013020834, 4: 1 - 1 = 0.
    expected <- c(0.393229167, 0.013020834, 0.0)

    result <- ppois(n = 4, theta = 0.5, val = c(0, 2, 4), log.p = FALSE, lower.tail = FALSE)
    expect_equal(result, expected, tolerance = 1e-8)
})

test_that("ppois returns logarithmic cumulative probabilities when log.p is TRUE", {
    # For a fixed set of values, check that log ppois equals the log of normal ppois.
    values <- c(0, 2, 4)
    result_normal <- ppois(n = 4, theta = 0.5, val = values, log.p = FALSE, lower.tail = TRUE)
    result_log    <- ppois(n = 4, theta = 0.5, val = values, log.p = TRUE, lower.tail = TRUE)

    expect_equal(result_log, log(result_normal), tolerance = 1e-8)
})

test_that("ppois returns NULL for invalid val input", {
    # For n = 4, valid values are 0 through 4. Providing a value like 5 should yield NULL.
    expect_null(ppois(n = 4, theta = 0.5, val = 5, log.p = FALSE, lower.tail = TRUE))
})
