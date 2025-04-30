
test_that("getLogarithmicMFPS returns a numeric vector of length 2 and lower < upper for valid n", {
    result <- getLogarithmicMFPS(2)

    # Check result is numeric, length 2, and lower < upper.
    # Check that the result is numeric and has exactly two elements.
    expect_type(result, "double")
    expect_length(result, 2)

    # Check that the lower limit is less than the upper limit.
    expect_true(result[1] < result[2], info = paste("Lower bound", result[1], "is not less than upper bound", result[2]))
})

test_that("getLogarithmicMFPS returns expected values for n = 2", {
    # For n = 2, the expected MFPS (rounded to 7 decimals) is [0.0000000, 0.4404231].
    expected <- c(0.0000000, 0.4404231)
    res <- getLogarithmicMFPS(2)
    expect_equal(round(res, 7), expected, info = paste("Expected:", toString(expected), "| Got:", toString(round(res, 7))))
})

test_that("getLogarithmicMFPS returns expected values for n = 3", {
    # For n = 3, the expected MFPS is [0.0000081, 0.3115801] (rounded to 7 decimals).
    expected <- c(0.0000081, 0.3115801)
    res <- getLogarithmicMFPS(3)
    expect_equal(round(res, 7), expected, info = paste("Expected:", toString(expected), "| Got:", toString(round(res, 7))))
})

test_that("getLogarithmicMFPS returns expected values for n = 4", {
    # For n = 4, the expected MFPS is [0.0001775, 0.2397019] (rounded to 7 decimals).
    expected <- c(0.0001775, 0.2397019)
    res <- getLogarithmicMFPS(4)
    expect_equal(round(res, 7), expected, info = paste("Expected:", toString(expected), "| Got:", toString(round(res, 7))))
})

test_that("getLogarithmicMFPS returns expected truncated values for n = 7", {
    # For n = 7, we check by scaling the output by 1e3 and truncating,
    # expecting the result to equal c(8, 140).
    expected = c(8, 140)
    res <- getLogarithmicMFPS(7)
    expect_equal(trunc(res * 1e3), expected, info = paste("Expected:", toString(expected), "| Got:", toString(trunc(res * 1e3))))
})

test_that("getLogarithmicMFPS returns NULL for non-integer n", {
    # Providing a non-integer for n should trigger an input validation.
    expect_null(getLogarithmicMFPS(2.5))
})

test_that("getLogarithmicMFPS returns NULL when n is missing", {
    # When n is not provided, the function should return invisible(NULL).
    expect_null(suppressMessages(getLogarithmicMFPS()), info = "Expected NULL when 'n' is missing")
})


test_that("getLogarithmicMFPS returns NULL when non-integer values are provided", {
    inputs <- c(2.5, 4.5)
    for (args in inputs) {
        expect_null(do.call(getLogarithmicMFPS, as.list(args)),
                    info = paste("Expected NULL for non-integer input:", toString(args)))
    }
    # Passing a non-integer for n should trigger input validation.
})
