test_that("ppois returns a data frame with full support when val is NULL", {
    expected_cdf <- c(0.606770833, 0.908854166, 0.986979166, 0.997395833, 1.0)

    result <- ppois(n = 4, theta = 0.5, val = NULL, log.p = FALSE, lower.tail = TRUE)

    expect_s3_class(result, "data.frame")
    expect_true(all(c("val", "cdf") %in% names(result)),
                info = "Data frame should contain 'val' and 'cdf' columns")

    expect_equal(result$val, 0:4, info = "Support should include values from 0 to 4 (n = 4)")

    expect_equal(result$cdf, expected_cdf, tolerance = 1e-8,
                 info = paste("CDF values differ from expected:\nExpected:", toString(expected_cdf),
                              "\nGot:", toString(result$cdf)))
})

test_that("ppois returns correct lower-tail probabilities for specified val", {
    expected <- c(0.606770833, 0.986979166, 1.0)
    val <- c(0, 2, 4)

    result <- ppois(n = 4, theta = 0.5, val = val, log.p = FALSE, lower.tail = TRUE)

    expect_equal(result, expected, tolerance = 1e-8,
                 info = paste("Lower-tail CDF mismatch:\nExpected:", toString(expected),
                              "\nGot:", toString(result)))
})

test_that("ppois returns correct upper-tail probabilities when lower.tail is FALSE", {
    expected <- c(0.393229167, 0.013020834, 0.0)
    val <- c(0, 2, 4)

    result <- ppois(n = 4, theta = 0.5, val = val, log.p = FALSE, lower.tail = FALSE)

    expect_equal(result, expected, tolerance = 1e-8,
                 info = paste("Upper-tail probabilities mismatch:\nExpected:", toString(expected),
                              "\nGot:", toString(result)))
})

test_that("ppois returns logarithmic cumulative probabilities when log.p is TRUE", {
    val <- c(0, 2, 4)

    result_normal <- ppois(n = 4, theta = 0.5, val = val, log.p = FALSE, lower.tail = TRUE)
    result_log    <- ppois(n = 4, theta = 0.5, val = val, log.p = TRUE, lower.tail = TRUE)

    expect_equal(result_log, log(result_normal), tolerance = 1e-8,
                 info = "Logarithmic CDF values should equal log of normal probabilities")
})

test_that("ppois returns NULL for invalid val input", {
    res <- ppois(n = 4, theta = 0.5, val = 5, log.p = FALSE, lower.tail = TRUE)

    expect_null(res, info = "Expected NULL for val = 5, which is outside support for n = 4")
})
