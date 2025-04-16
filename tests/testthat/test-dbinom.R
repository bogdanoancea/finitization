

test_that("dbinom computes correct density values for n = 2", {
    # Expected density values for finitization order n = 2, p = 0.15, N = 4.
    expected <- data.frame(
        val  = c(0, 1, 2),
        prob = c(0.535, 0.330, 0.135)
    )
    result <- dbinom(n = 2, p = 0.15, N = 4)

    # Check that the function returns a data frame with the correct column names.
    expect_s3_class(result, "data.frame")
    expect_equal(names(result), c("val", "prob"))

    # Compare the computed values with the expected values using a tolerance.
    expect_equal(result, expected, tolerance = 1e-3)
})

test_that("dbinom computes correct density values for n = 4", {
    # Expected density values for finitization order n = 4, p = 0.15, N = 4.
    expected <- data.frame(
        val  = c(0, 1, 2, 3, 4),
        prob = c(0.52200625, 0.36847500, 0.09753750, 0.01147500, 0.00050625)
    )
    result <- dbinom(n = 4, p = 0.15, N = 4)

    expect_s3_class(result, "data.frame")
    expect_equal(names(result), c("val", "prob"))

    # Use a stricter tolerance for these computed values.
    expect_equal(result, expected, tolerance = 1e-8)
})

test_that("dbinom returns NULL for invalid 'val' input", {
    # For n = 4, valid outcomes are 0 to 4. Providing an invalid outcome (e.g. 5) should return NULL.
    expect_null(dbinom(n = 4, p = 0.15, N = 4, val = 5))
})

test_that("dbinom computes correct density for a single value", {
    # Expected output when a single outcome is provided.
    expected <- data.frame(
        val  = 3,
        prob = 0.0135
    )
    result <- dbinom(n = 3, p = 0.15, N = 4, val = 3)

    expect_s3_class(result, "data.frame")
    expect_equal(names(result), c("val", "prob"))

    # Compare using a tolerance.
    expect_equal(result, expected, tolerance = 1e-4)
})

test_that("dbinom returns log-densities when log = TRUE", {
    # Compute the densities with log = FALSE.
    result_normal <- dbinom(n = 4, p = 0.5, N = 4, log = FALSE)

    # Compute the densities with log = TRUE.
    result_log <- dbinom(n = 4, p = 0.5, N = 4, log = TRUE)

    # Compare that the log of the normal densities equals the computed log densities.
    expect_equal(log(result_normal$prob), result_log$prob, tolerance = 1e-8)
})
