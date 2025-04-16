test_that("binomial MFPS", {
  expect_equal(round(getBinomialMFPS(2,4), 7), c(0.0000000, 0.3333333) )
})

test_that("getBinomialMFPS returns a numeric vector of length 2 for valid inputs", {
    # For example, using n = 2 and N = 4.
    result <- getBinomialMFPS(2, 4)

    # Check that the result is numeric and has exactly two elements.
    expect_type(result, "double")
    expect_length(result, 2)

    # Check that the lower limit is less than the upper limit.
    expect_true(result[1] < result[2])
})

test_that("getBinomialMFPS returns NULL when non-integer values are provided", {
    # Passing a non-integer for n should trigger input validation.
    expect_null(getBinomialMFPS(2.5, 4))

    # Passing a non-integer for N should trigger input validation.
    expect_null(getBinomialMFPS(2, 4.5))
})

test_that("getBinomialMFPS returns NULL when a required argument is missing", {
    # When n is missing:
    expect_null(getBinomialMFPS(N = 4))

    # When N is missing:
    expect_null(getBinomialMFPS(2))
})

