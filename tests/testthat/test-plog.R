test_that("plog returns full CDF table when val is NULL", {
    n <- 4
    theta <- 0.1
    res <- plog(n = n, theta = theta, val = NULL, log.p = FALSE, lower.tail = TRUE)

    expect_s3_class(res, "data.frame")
    expect_true(all(c("val", "cdf") %in% names(res)), info = "Expected columns 'val' and 'cdf' in result")
    expect_equal(res$val, 0:n, info = "Expected support to range from 0 to n")

    expect_true(all(diff(res$cdf) >= 0), info = "CDF should be non-decreasing")
    expect_true(all(res$cdf >= 0 & res$cdf <= 1), info = "CDF values should lie in [0, 1]")
    expect_equal(res$cdf[length(res$cdf)], 1, tolerance = 1e-8,
                 info = "Last value of CDF should be approximately 1")
})

test_that("plog returns correct lower-tail probabilities for specified val", {
    n <- 4
    theta <- 0.1
    vals <- c(0, 2, 4)
    res <- plog(n = n, theta = theta, val = vals, log.p = FALSE, lower.tail = TRUE)

    expect_s3_class(res, "data.frame")
    expect_true(all(c("val", "cdf") %in% names(res)), info = "Expected columns 'val' and 'cdf' in result")

    expect_equal(nrow(res), length(vals))
    expect_true(all(res$cdf >= 0 & res$cdf <= 1), info = "All CDF values should be in [0, 1]")
    expect_equal(res[nrow(res),]$cdf, 1, tolerance = 1e-8,
                 info = "CDF at maximum value (n) should be ~1")
})

test_that("plog returns correct upper-tail probabilities", {
    n <- 4
    theta <- 0.1
    vals <- c(0, 2, 4)

    lower <- plog(n = n, theta = theta, val = vals, log.p = FALSE, lower.tail = TRUE)
    upper <- plog(n = n, theta = theta, val = vals, log.p = FALSE, lower.tail = FALSE)

    expect_s3_class(lower, "data.frame")
    expect_s3_class(upper, "data.frame")
    expect_true(all(c("val", "cdf") %in% names(lower)), info = "Expected columns 'val' and 'cdf' in result")
    expect_true(all(c("val", "cdf") %in% names(upper)), info = "Expected columns 'val' and 'cdf' in result")

    expect_equal(upper$cdf, 1 - lower$cdf, tolerance = 1e-8,
                 info = "Upper tail should equal 1 - lower tail for each val")
})

test_that("plog returns log probabilities when log.p = TRUE", {
    n <- 4
    theta <- 0.1
    vals <- c(0, 2, 4)

    normal <- plog(n = n, theta = theta, val = vals, log.p = FALSE, lower.tail = TRUE)
    logvals <- plog(n = n, theta = theta, val = vals, log.p = TRUE, lower.tail = TRUE)

    expect_s3_class(normal, "data.frame")
    expect_s3_class(logvals, "data.frame")
    expect_true(all(c("val", "cdf") %in% names(normal)), info = "Expected columns 'val' and 'cdf' in result")
    expect_true(all(c("val", "cdf") %in% names(logvals)), info = "Expected columns 'val' and 'cdf' in result")
    expect_equal(logvals$cdf, log(normal$cdf), tolerance = 1e-8,
                 info = "Log-probabilities should match log of normal probabilities")
})

test_that("plog returns NULL for invalid val input", {
    n <- 4
    theta <- 0.1
    res <- plog(n = n, theta = theta, val = 5, log.p = FALSE, lower.tail = TRUE)

    expect_null(res, info = "Expected NULL for val = 5, which is outside support for n = 4")
})
