

test_that("qbinom returns increasing quantiles and handles edge cases", {
    # Define a set of probabilities: including 0 and 1 for the edge cases.
    probs <- c(0, 0.1, 0.5, 0.9, 1)

    # Using a simple finitization: n = 4 (support: 0, 1, 2, 3, 4), p = 0.5, N = 4.
    quantiles <- qbinom(n = 4, p = 0.5, N = 4, prob = probs, lower.tail = TRUE, log.p = FALSE)

    # Check that the output is numeric.
    expect_type(quantiles, "double")

    # Check that the quantiles are monotonically non-decreasing.
    expect_true(all(diff(quantiles) >= 0))

    # Check edge cases: p = 0 should return 0, p = 1 should return the maximum value (which is 4).
    expect_equal(quantiles[1], 0)
    expect_equal(quantiles[length(quantiles)], 4)
})

test_that("qbinom handles log-scale probabilities correctly", {
    # Define a set of probabilities.
    probs <- c(0.2, 0.5, 0.8)

    # Compute quantiles with standard probabilities.
    quantiles_standard <- qbinom(n = 4, p = 0.5, N = 4, prob = probs, lower.tail = TRUE, log.p = FALSE)

    # Compute quantiles with logged probabilities.
    quantiles_log <- qbinom(n = 4, p = 0.5, N = 4, prob = log(probs), lower.tail = TRUE, log.p = TRUE)

    expect_equal(quantiles_standard, quantiles_log)
})

test_that("qbinom handles lower.tail parameter correctly", {
    # Define a set of probabilities.
    probs <- c(0.1, 0.3, 0.7)

    # Compute quantiles for lower-tail probabilities.
    quantiles_lower <- qbinom(n = 4, p = 0.5, N = 4, prob = probs, lower.tail = TRUE, log.p = FALSE)

    # For upper-tail probabilities, we use (1 - p) and set lower.tail = FALSE.
    quantiles_upper <- qbinom(n = 4, p = 0.5, N = 4, prob = 1 - probs, lower.tail = FALSE, log.p = FALSE)

    expect_equal(quantiles_lower, quantiles_upper)
})
