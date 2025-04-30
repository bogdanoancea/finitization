test_that("dbinom computes correct density values for n = 2", {
    # Expected density values for finitization order n = 2, p = 0.15, N = 4.
    expected <- data.frame(
        val  = c(0, 1, 2),
        prob = c(0.535, 0.330, 0.135)
    )
    result <- dbinom(n = 2, p = 0.15, N = 4)
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

test_that("dbinom computes correct density values for n = 4", {
    # Expected density values for finitization order n = 4, p = 0.15, N = 4.
    expected <- data.frame(
        val  = c(0, 1, 2, 3, 4),
        prob = c(0.52200625, 0.36847500, 0.09753750, 0.01147500, 0.00050625)
    )
    result <- dbinom(n = 4, p = 0.15, N = 4)

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

test_that("dbinom returns NULL for invalid 'val' input", {
    invalid_vals <- list(5, -1, 10)

    for (iv in invalid_vals) {
        msg <- paste("Expected NULL for val =", deparse(iv))
        expect_null(
            suppressMessages(dbinom(n = 4, p = 0.15, N = 4, val = iv)),
            info = msg
        )
    }
})


test_that("dbinom computes correct density for a single value", {
    # Expected output when a single outcome is provided.
    expected <- data.frame(
        val  = 3,
        prob = 0.0135
    )
    result <- dbinom(n = 3, p = 0.15, N = 4, val = 3)

    expect_s3_class(result, "data.frame")
    expect_equal(result$val, expected$val)
    expect_equal(result$prob, expected$prob, tolerance = 1e-6)
})

test_that("dbinom returns log-densities when log = TRUE", {
    # Compute the densities with log = FALSE.
    result_normal <- dbinom(n = 4, p = 0.5, N = 4, log = FALSE)

    # Compute the densities with log = TRUE.
    result_log <- dbinom(n = 4, p = 0.5, N = 4, log = TRUE)

    # Compare that the log of the normal densities equals the computed log densities.
    expect_equal(log(result_normal$prob), result_log$prob, tolerance = 1e-8)
})
