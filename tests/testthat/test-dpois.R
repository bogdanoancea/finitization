test_that("dpois produces correct densities for all outcomes (val not provided)", {
    # Expected data for n = 4, theta = 0.5 over outcomes 0:4.
    expected <- data.frame(
        val  = 0:4,
        prob = c(0.606770833, 0.302083333, 0.078125, 0.010416667, 0.002604167)
    )
    result <- dpois(n = 4, theta = 0.5)

    # Verify the result is a data frame with correct column names.
    expect_s3_class(result, "data.frame")
    expect_equal(names(result), c("val", "prob"))

    # Check that the outcomes and densities match (using a tolerance appropriate for 9 decimal places).
    expect_equal(result$val, expected$val)
    expect_equal(result$prob, expected$prob)
})

test_that("dpois returns NULL for an invalid 'val' input", {
    # For n = 7, the valid outcomes are 0:7.
    # Providing val = 9 should trigger an input validation error and result in NULL.
    expect_null(dpois(n = 7, theta = 0.15, val = 9))
})

test_that("dpois produces correct density for a single value", {
    # For n = 7, theta = 0.15 and a single outcome (val = 4),
    # the expected result is a data frame with one row.
    expected <- data.frame(val = 4, prob = 1.815513e-05)
    result <- dpois(n = 7, theta = 0.15, val = 4)

    # Check the returned object structure.
    expect_s3_class(result, "data.frame")
    expect_equal(names(result), c("val", "prob"))

    # Compare the computed density to the expected value with a tight tolerance.
    expect_equal(result$val, expected$val)
    expect_equal(result$prob, expected$prob, tolerance = 1e-6)
})

test_that("dpois returns log densities when log = TRUE", {
    # Compute the densities in normal and log-scale modes.
    result_normal <- dpois(n = 4, theta = 0.5, val = 0:4, log = FALSE)
    result_log    <- dpois(n = 4, theta = 0.5, val = 0:4, log = TRUE)

    # Check that the log of the normal densities equals the log-mode densities.
    expect_equal(result_log$prob, log(result_normal$prob), tolerance = 1e-9)
})
