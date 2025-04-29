test_that("pnegbinom returns a data frame with full support when val is NULL", {
    expected_cdf <- c(0.6018843, 0.8791707, 0.9855927, 0.9878979, 1.00000000)


    result <- pnegbinom(n = 4, q = 0.12, k = 4, val = NULL, log.p = FALSE, lower.tail = TRUE)

    expect_s3_class(result, "data.frame")
    expect_true(all(c("val", "cdf") %in% names(result)),
                info = "Data frame should contain 'val' and 'cdf' columns")

    expect_equal(result$val, 0:4, info = "Support should include values from 0 to 4 (n = 4)")

    expect_equal(result$cdf, expected_cdf, tolerance = 1e-6,
                 info = paste("CDF values differ from expected:\nExpected:", toString(expected_cdf),
                              "\nGot:", toString(result$cdf)))
})

test_that("pnegbinom returns correct lower-tail probabilities for specified val", {
    expected <- c(0.6018843, 0.9855927, 1.00000000)  # Example values
    val <- c(0, 2, 4)

    result <- pnegbinom(n = 4, q = 0.12, k = 4, val = val, log.p = FALSE, lower.tail = TRUE)
    expect_s3_class(result, "data.frame")
    expect_true(all(c("val", "cdf") %in% names(result)),
                info = "Data frame should contain 'val' and 'cdf' columns")

    expect_equal(result$cdf, expected, tolerance = 1e-6,
                 info = paste("Lower-tail CDF mismatch:\nExpected:", toString(expected),
                              "\nGot:", toString(result$cdf)))
})

test_that("pnegbinom returns correct upper-tail probabilities when lower.tail is FALSE", {
    expected <- c(3.981157e-01, 1.440732e-02, 2.220446e-16)  # Example values
    val <- c(0, 2, 4)

    result <- pnegbinom(n = 4, q = 0.12, k = 4, val = val, log.p = FALSE, lower.tail = FALSE)
    expect_s3_class(result, "data.frame")
    expect_true(all(c("val", "cdf") %in% names(result)),
                info = "Data frame should contain 'val' and 'cdf' columns")

    expect_equal(result$cdf, expected, tolerance = 1e-6,
                 info = paste("Upper-tail probabilities mismatch:\nExpected:", toString(expected),
                              "\nGot:", toString(result$cdf)))
})

test_that("pnegbinom returns logarithmic cumulative probabilities when log.p is TRUE", {
    val <- c(0, 2, 4)

    result_normal <- pnegbinom(n = 4, q = 0.12, k = 4, val = val, log.p = FALSE, lower.tail = TRUE)
    result_log    <- pnegbinom(n = 4, q = 0.12, k = 4, val = val, log.p = TRUE, lower.tail = TRUE)
    expect_s3_class(result_normal, "data.frame")
    expect_s3_class(result_log, "data.frame")
    expect_true(all(c("val", "cdf") %in% names(result_normal)),
                info = "Data frame should contain 'val' and 'cdf' columns")
    expect_true(all(c("val", "cdf") %in% names(result_log)),
                info = "Data frame should contain 'val' and 'cdf' columns")
    expect_equal(result_log$cdf, log(result_normal$cdf), tolerance = 1e-6,
                 info = "Logarithmic CDF values should equal log of normal probabilities")
})

test_that("pnegbinom returns NULL for invalid val input", {
    res <- pnegbinom(n = 4, q = 0.12, k = 4, val = 5, log.p = FALSE, lower.tail = TRUE)

    expect_null(res, info = "Expected NULL for val = 5, which is outside support for n = 4")
})
