test_that("qnegbinomial returns a numeric vector of correct length and values within support", {
    n <- 4
    q <- 0.12
    k <- 4
    probs <- c(0, 0.25, 0.5, 0.75, 1)

    result <- qnegbinom(n = n, q = q, k = k, p = probs, lower.tail = TRUE, log.p = FALSE)

    # Check output is numeric and has the same length as probs.
    expect_type(result, "integer")
    expect_length(result, length(probs))

    # All quantiles should be between 0 and n (inclusive).
    expect_true(all(result >= 0 & result <= n))
})

test_that("qnegbinomial is monotonic increasing with increasing probabilities (lower.tail = TRUE)", {
    n <- 4
    q <- 0.12
    k <- 4
    # Create a sequence of probabilities from 0 to 1.
    probs <- seq(0, 1, length.out = 10)

    result <- qnegbinom(n = n, q = q, k = k, p = probs, lower.tail = TRUE, log.p = FALSE)
    # Check that the quantiles are non-decreasing.
    expect_true(all(diff(result) >= 0))
})

test_that("qnegbinomial upper-tail conversion is consistent", {
    n <- 4
    q <- 0.12
    k <- 4
    probs <- c(0.1, 0.3, 0.7)

    # For upper-tail: qnegbinomial with lower.tail = FALSE should equal
    # qnegbinomial(n, q, k, p = (1 - p), lower.tail = TRUE).
    quantiles_upper <- qnegbinom(n = n, q = q, k = k, p = probs, lower.tail = FALSE, log.p = FALSE)
    quantiles_lower <- qnegbinom(n = n, q = q, k = k, p = (1 - probs), lower.tail = TRUE, log.p = FALSE)

    expect_equal(quantiles_upper, quantiles_lower, tolerance = 1e-8)
})

test_that("qnegbinomial handles log-scale probabilities correctly", {
    n <- 4
    q <- 0.12
    k <- 4
    probs <- c(0.2, 0.4, 0.6)

    quantiles_standard <- qnegbinom(n = n, q = q, k = k, p = probs, lower.tail = TRUE, log.p = FALSE)
    quantiles_log      <- qnegbinom(n = n, q = q, k = k, p = log(probs), lower.tail = TRUE, log.p = TRUE)

    expect_equal(quantiles_standard, quantiles_log, tolerance = 1e-8)
})

test_that("qnegbinomial errors when probabilities are outside [0,1]", {
    n <- 4
    q <- 0.12
    k <- 4

    # Negative probability.
    expect_error(
        qnegbinom(n = n, q = q, k = k, p = c(-0.1, 0.5), lower.tail = TRUE, log.p = FALSE),
        "Probabilities in 'p' must be between 0 and 1"
    )

    # Probability greater than 1.
    expect_error(
        qnegbinom(n = n, q = q, k = k, p = c(0.5, 1.1), lower.tail = TRUE, log.p = FALSE),
        "Probabilities in 'p' must be between 0 and 1"
    )
})

test_that("qnegbinomial returns invisible NULL when a required argument is missing", {
    # For example, omitting q should result in invisible NULL.
    expect_null(suppressMessages(qnegbinom(n = 4, k = 4, p = c(0.5), lower.tail = TRUE, log.p = FALSE)))
})
