test_that("getNegativeBinomialMFPS returns a numeric vector of length 2 for valid inputs", {
    res <- getNegativeBinomialMFPS(2, 4)

    # Check that the output is numeric and has exactly 2 elements.
    expect_type(res, "double")
    expect_length(res, 2)

    # Verify that the lower limit is strictly less than the upper limit.
    expect_true(res[1] < res[2])
})

test_that("getNegativeBinomialMFPS returns expected values for n = 2, k = 4", {
    # For n = 2 and k = 4, we expect the maximum feasible parameter space to be [0, 0.1666667] (rounded to 7 decimals).
    res <- getNegativeBinomialMFPS(2, 4)
    expected <- c(0.0000000, 0.1666667)

    expect_equal(round(res, 7), expected)
})

test_that("getNegativeBinomialMFPS returns expected values for n = 3, k = 4", {
    # For n = 3 and k = 4, we expect the maximum feasible parameter space to be [0, 0.1428571] (rounded to 7 decimals).
    res <- getNegativeBinomialMFPS(3, 4)
    expected <- c(0.0000000, 0.1428571)

    expect_equal(round(res, 7), expected)
})

test_that("getNegativeBinomialMFPS returns NULL for non-integer inputs", {
    # Using non-integer values for n or k should fail the input check.
    expect_null(getNegativeBinomialMFPS(2.5, 4))
    expect_null(getNegativeBinomialMFPS(3, 3.7))
})

test_that("getNegativeBinomialMFPS returns NULL if required arguments are missing", {
    # Missing n.
    expect_null(getNegativeBinomialMFPS(k = 4))
    # Missing k.
    expect_null(getNegativeBinomialMFPS(n = 2))
})
