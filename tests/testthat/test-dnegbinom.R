test_that("dnegbinom returns correct densities for n = 2, q = 0.15, k = 4", {
    expected <- data.frame(
        val  = c(0, 1, 2),
        prob = c(0.60553633, 0.08304498, 0.31141869)
    )

    result <- dnegbinom(n = 2, q = 0.15, k = 4)

    # Check structure
    expect_s3_class(result, "data.frame")
    expect_named(result, c("val", "prob"))
    expect_type(result$val, "integer")
    expect_type(result$prob, "double")

    # Check values
    expect_equal(result$val, expected$val)

    expect_equal(
        result$prob, expected$prob,
        tolerance = 1e-8,
        info = paste(
            "Mismatch in probabilities:\nExpected: ", toString(expected$prob),
            "\nGot: ", toString(result$prob),
            "\nDiff: ", toString(result$prob - expected$prob)
        )
    )
})


test_that("dnegbinom returns correct densities for n = 4, q = 0.11, k = 4", {
    expected <- data.frame(
        val  = c(0, 1, 2, 3, 4),
        prob = c(0.628783247, 0.269477400, 0.088480741, 0.005091307, 0.008167305)
    )

    result <- dnegbinom(n = 4, q = 0.11, k = 4)

    # Structural checks
    expect_s3_class(result, "data.frame")
    expect_named(result, c("val", "prob"))
    expect_type(result$val, "integer")
    expect_type(result$prob, "double")

    # Value checks
    expect_equal(result$val, expected$val)

    expect_equal(
        result$prob, expected$prob,
        tolerance = 1e-8,
        info = paste(
            "Mismatch in probabilities:\nExpected: ", toString(expected$prob),
            "\nGot: ", toString(result$prob),
            "\nDiff: ", toString(result$prob - expected$prob)
        )
    )

    # Check if probabilities sum to 1 within tolerance
    prob_sum <- sum(result$prob)
    expect_equal(prob_sum, 1, tolerance = 1e-8,
                 info = paste("Probabilities do not sum to 1. Total:", prob_sum))
})


test_that("dnegbinom returns NULL for invalid 'val' input", {
    invalid_vals <- list(5, -1, 10)

    for (iv in invalid_vals) {
        msg <- paste("Expected NULL for val =", deparse(iv))
        expect_null(
            dnegbinom(n = 4, q = 0.11, k = 4, val = iv),
            info = msg
        )
    }
})

test_that("dnegbinom returns correct density for a single value", {
    # For n = 3, q = 0.15, k = 4 and val = 3, the expected density is approximately 0.1099125.
    expected <- data.frame(
        val  = 3,
        prob = 0.1099125
    )
    result <- dnegbinom(n = 3, q = 0.15, k = 4, val = 3)

    expect_s3_class(result, "data.frame")
    expect_equal(result$val, expected$val)
    expect_equal(result$prob, expected$prob, tolerance = 1e-6)
})

test_that("dnegbinom returns log densities when log = TRUE", {
    # Compare the log densities with the log of the computed densities.
    result_normal <- dnegbinom(n = 2, q = 0.15, k = 4, log = FALSE)
    result_log    <- dnegbinom(n = 2, q = 0.15, k = 4, log = TRUE)

    expect_equal(result_log$prob, log(result_normal$prob), tolerance = 1e-8)
})
