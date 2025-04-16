library(testthat)
library(finitization)

test_that("qlog returns a numeric vector of correct length and values within support", {
    n <- 4
    theta <- 0.1
    p_vals <- c(0, 0.25, 0.5, 0.75, 1)

    res <- qlog(n = n, theta = theta, p = p_vals, lower.tail = TRUE, log.p = FALSE)

    # Check that the output is numeric and of the expected length.
    expect_type(res, "integer")
    expect_length(res, length(p_vals))

    # Ensure that each quantile is within the valid support [0, n].
    expect_true(all(res >= 0 & res <= n))
})

test_that("qlog is monotonic non-decreasing with increasing p", {
    n <- 4
    theta <- 0.1
    p_vals <- seq(0, 1, length.out = 10)

    res <- qlog(n = n, theta = theta, p = p_vals, lower.tail = TRUE, log.p = FALSE)

    # The quantiles should be non-decreasing as p increases.
    expect_true(all(diff(res) >= 0))
})

test_that("qlog returns correct extreme quantiles", {
    n <- 4
    theta <- 0.1

    # For p = 0, quantile should be 0.
    q0 <- qlog(n = n, theta = theta, p = 0, lower.tail = TRUE, log.p = FALSE)
    expect_equal(q0, 0)

    # For p = 1, quantile should be n.
    q1 <- qlog(n = n, theta = theta, p = 1, lower.tail = TRUE, log.p = FALSE)
    expect_equal(q1, n)
})

test_that("qlog handles log-scale probabilities correctly", {
    n <- 4
    theta <- 0.1
    p_vals <- c(0.1, 0.5, 0.9)

    res_standard <- qlog(n = n, theta = theta, p = p_vals, lower.tail = TRUE, log.p = FALSE)
    res_log      <- qlog(n = n, theta = theta, p = log(p_vals), lower.tail = TRUE, log.p = TRUE)

    expect_equal(res_standard, res_log, tolerance = 1e-8)
})

test_that("qlog handles lower.tail flag correctly", {
    n <- 4
    theta <- 0.1
    p_vals <- c(0.1, 0.5, 0.9)

    # When lower.tail = FALSE, the function should yield the same quantiles as using 1 - p with lower.tail = TRUE.
    res_lower <- qlog(n = n, theta = theta, p = p_vals, lower.tail = TRUE, log.p = FALSE)
    res_upper <- qlog(n = n, theta = theta, p = 1 - p_vals, lower.tail = FALSE, log.p = FALSE)

    expect_equal(res_lower, res_upper, tolerance = 1e-8)
})

test_that("qlog errors for probabilities outside [0,1]", {
    n <- 4
    theta <- 0.1

    expect_error(qlog(n = n, theta = theta, p = c(-0.1, 0.5), lower.tail = TRUE, log.p = FALSE),
                 "Probabilities in 'p' must be between 0 and 1")

    expect_error(qlog(n = n, theta = theta, p = c(0.5, 1.1), lower.tail = TRUE, log.p = FALSE),
                 "Probabilities in 'p' must be between 0 and 1")
})
