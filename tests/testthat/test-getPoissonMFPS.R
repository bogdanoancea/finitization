test_that("getPoissonMFPS returns a numeric vector of length 2 with lower < upper for valid n", {
    result <- getPoissonMFPS(7)

    # Check that the result is numeric and has exactly two elements.
    expect_type(result, "double")
    expect_length(result, 2)

    # Check that the lower limit is less than the upper limit.
    expect_true(result[1] < result[2], info = paste("Lower bound", result[1], "is not less than upper bound", result[2]))

    # For the finitized Poisson distribution, the MFPS is expected to be [0,1].
    expect_equal(result, c(0, 1))
})

test_that("getPoissonMFPS returns the expected result for n = 1", {
    result <- getPoissonMFPS(1)
    expected <- c(1, 1)

    # Structure checks
    expect_type(result, "double")
    expect_length(result, 2)

    # Value checks
    expect_equal(result, expected, tolerance = 1e-8,
                 info = paste("Expected Poisson MFPS to be", toString(expected),
                              "but got", toString(result)))

    # Semantic check: valid parameter space [0, 1]
    expect_true(result[1] >= 0 && result[2] <= 1,
                info = paste("Bounds are out of [0,1]:", toString(result)))
})



test_that("getPoissonMFPS returns NULL when a non-integer n is provided", {
    # A non-integer n (like 2.5) should fail the input check and yield NULL.
    #expect_null(getPoissonMFPS(2.5))
    inputs <- c(2.5, 4.5)
    for (args in inputs) {
        expect_null(do.call(getPoissonMFPS, as.list(args)),
                    info = paste("Expected NULL for non-integer input:", toString(args)))
    }


})

test_that("getPoissonMFPS returns NULL when n is missing", {
    # When n is missing the function should warn and return NULL.
    expect_null(suppressMessages(getPoissonMFPS()))
})

test_that("parameters within MFPS produce valid Poisson samples", {
    n <- 5
    lambda <- 0.8   # inside MFPS for Poisson
    no <- 1000

    mfps <- getPoissonMFPS(n)

    # Verify lambda is within MFPS
    expect_true(lambda >= mfps[1] && lambda <= mfps[2])

    # Should successfully generate samples
    result <- rpois(n, lambda, no)

    expect_equal(length(result), no)
    expect_true(all(result >= 0))
    expect_true(all(result <= n))
})

