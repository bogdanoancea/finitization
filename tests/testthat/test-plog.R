
# Test that when val is not provided, plog returns a full data frame covering the support.
test_that("plog returns full CDF table when val is NULL", {
    n <- 4
    theta <- 0.1

    res <- plog(n = n, theta = theta, val = NULL, log.p = FALSE, lower.tail = TRUE)

    # Check that a data frame is returned with the expected column names.
    expect_s3_class(res, "data.frame")
    expect_true(all(c("val", "cdf") %in% names(res)))

    # The support should be 0 to n.
    expect_equal(res$val, 0:n)

    # The cumulative probabilities should be non-decreasing and lie in [0,1].
    expect_true(all(diff(res$cdf) >= 0))
    expect_true(all(res$cdf >= 0 & res$cdf <= 1))

    # Last value is approximately 1.
    expect_equal(res$cdf[length(res$cdf)], 1, tolerance = 1e-8)
})

# Test that plog returns a numeric vector with the expected length when val is provided.
test_that("plog returns correct lower-tail probabilities for specified val", {
    n <- 4
    theta <- 0.1
    vals <- c(0, 2, 4)

    res <- plog(n = n, theta = theta, val = vals, log.p = FALSE, lower.tail = TRUE)

    # Check that the output is a numeric vector of the expected length.
    expect_type(res, "double")
    expect_length(res, length(vals))

    # Check that the probabilities are between 0 and 1.
    expect_true(all(res >= 0 & res <= 1))

    # The cumulative probability at the maximum outcome (val = n) should be 1.
    expect_equal(res[length(res)], 1, tolerance = 1e-8)
})

# Test that upper-tail mode returns the complement of lower-tail.
test_that("plog returns correct upper-tail probabilities", {
    n <- 4
    theta <- 0.1
    vals <- c(0, 2, 4)

    lower <- plog(n = n, theta = theta, val = vals, log.p = FALSE, lower.tail = TRUE)
    upper <- plog(n = n, theta = theta, val = vals, log.p = FALSE, lower.tail = FALSE)

    # For each value, upper-tail probability should equal 1 minus lower-tail probability.
    expect_equal(upper, 1 - lower, tolerance = 1e-8)
})

# Test that log.p returns logarithms of the probabilities.
test_that("plog returns log probabilities when log.p is TRUE", {
    n <- 4
    theta <- 0.1
    vals <- c(0, 2, 4)

    res_normal <- plog(n = n, theta = theta, val = vals, log.p = FALSE, lower.tail = TRUE)
    res_log    <- plog(n = n, theta = theta, val = vals, log.p = TRUE, lower.tail = TRUE)

    expect_equal(res_log, log(res_normal), tolerance = 1e-8)
})

# Test that plog returns NULL when an invalid val is provided.
test_that("plog returns NULL for invalid val input", {
    n <- 4
    theta <- 0.1
    # Valid outcomes are 0,1,...,4. An invalid value like 5 should yield NULL.
    expect_null(plog(n = n, theta = theta, val = 5, log.p = FALSE, lower.tail = TRUE))
})
