test_that("getBinomialMFPS returns a numeric vector of length 2 for valid inputs", {
    # For example, using n = 2 and N = 4.
    result <- getBinomialMFPS(2, 4)

    # Check that the result is numeric and has exactly two elements.
    expect_type(result, "double")
    expect_length(result, 2)

    # Check that the lower limit is less than the upper limit.
    expect_true(result[1] < result[2], info = paste("Lower bound", result[1], "is not less than upper bound", result[2]))
})


test_that("binomial MFPS", {
    expected <- c(0.0000000, 0.3333333)
    res <- getBinomialMFPS(2, 4)
    expect_equal(round(res, 7), expected, info = paste("Expected:", toString(expected), "| Got:", toString(round(res, 7))))
})


test_that("getBinomialMFPS returns NULL when non-integer values are provided", {
    inputs <- list(c(2.5, 4), c(2, 4.5))
    for (args in inputs) {
        expect_null(do.call(getBinomialMFPS, as.list(args)),
                    info = paste("Expected NULL for non-integer input:", toString(args)))
    }
    # Passing a non-integer for n should trigger input validation.
})

test_that("getBinomialMFPS returns NULL when a required argument is missing", {
    expect_null(suppressMessages(getBinomialMFPS(N = 4)), info = "Expected NULL when 'n' is missing")
    expect_null(suppressMessages(getBinomialMFPS(2)), info = "Expected NULL when 'N' is missing")
})

