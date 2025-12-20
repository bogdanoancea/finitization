test_that("getNegativeBinomialMFPS returns numeric interval of length 2", {
    res <- getNegativeBinomialMFPS(2, 4)

    expect_type(res, "double")
    expect_length(res, 2)
    expect_true(res[1] < res[2], info = paste("Lower bound", res[1], "is not less than upper bound", res[2]))
})

test_that("getNegativeBinomialMFPS returns expected value for n = 2, k = 4", {
    expected <- c(0.0000000, 0.1666667)
    res <- getNegativeBinomialMFPS(2, 4)
    expect_equal(round(res, 7), expected, info = paste("Expected:", toString(expected), "| Got:", toString(round(res, 7))))
})

test_that("getNegativeBinomialMFPS returns expected value for n = 3, k = 4", {
    expected <- c(0.0000000, 0.1428571)
    res <- getNegativeBinomialMFPS(3, 4)
    expect_equal(round(res, 7), expected, info = paste("Expected:", toString(expected), "| Got:", toString(round(res, 7))))
})

test_that("getNegativeBinomialMFPS returns NULL for non-integer inputs", {
    inputs <- list(c(2.5, 4), c(3, 3.7))
    for (args in inputs) {
        expect_null(do.call(getNegativeBinomialMFPS, as.list(args)),
                    info = paste("Expected NULL for non-integer input:", toString(args)))
    }
})

test_that("getNegativeBinomialMFPS returns NULL when arguments are missing", {
    expect_null(suppressMessages(getNegativeBinomialMFPS(k = 4)), info = "Expected NULL when 'n' is missing")
    expect_null(suppressMessages(getNegativeBinomialMFPS(n = 2)), info = "Expected NULL when 'k' is missing")
})

test_that("parameters within MFPS produce valid negative binomial samples", {
    n <- 5
    k <- 2
    q <- 0.1
    no <- 1000

    mfps <- getNegativeBinomialMFPS(n, k)

    # Verify q is within MFPS
    expect_true(q >= mfps[1] && q <= mfps[2])

    # Should successfully generate samples
    result <- rnegbinom(n, q, k, no)

    expect_equal(length(result), no)
    expect_true(all(result >= 0))
    expect_true(all(result <= n))
})
