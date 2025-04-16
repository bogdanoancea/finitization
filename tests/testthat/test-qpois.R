test_that("qpois returns correct quantiles in lower-tail mode", {
    # Define a vector of probabilities.
    probs <- c(0, 0.5, 0.9, 0.95, 1)

    # Expected quantiles when interpreted as lower-tail.
    # For n = 4, theta = 0.5 and using the approximate cumulative probabilities:
    # F(0)=0.606770833, F(1)=0.908854166, F(2)=0.986979166, F(3)=0.997395833, F(4)=1
    # Therefore:
    # p = 0       -> quantile 0 (since F(0)=0.60677 >= 0)
    # p = 0.5     -> quantile 0 (since 0.60677 >= 0.5)
    # p = 0.9     -> quantile 1 (F(0) < 0.9, F(1)=0.90885>=0.9)
    # p = 0.95    -> quantile 2 (F(1) < 0.95, F(2)=0.98698>=0.95)
    # p = 1       -> quantile 4
    expected <- c(0, 0, 1, 2, 4)

    quantiles <- qpois(n = 4, theta = 0.5, p = probs, lower.tail = TRUE, log.p = FALSE)

    expect_type(quantiles, "integer")
    expect_length(quantiles, length(probs))
    expect_equal(quantiles, expected, tolerance = 1e-8)
})

test_that("qpois returns correct quantiles in upper-tail mode", {
    # For upper-tail probabilities, the function internally converts p <- 1 - p.
    # Hence, qpois(n, theta, p, lower.tail = FALSE) should equal
    # qpois(n, theta, 1 - p, lower.tail = TRUE)
    probs <- c(0.1, 0.5, 0.9)

    quantiles_upper <- qpois(n = 4, theta = 0.5, p = probs, lower.tail = FALSE, log.p = FALSE)
    quantiles_lower <- qpois(n = 4, theta = 0.5, p = 1 - probs, lower.tail = TRUE, log.p = FALSE)

    expect_equal(quantiles_upper, quantiles_lower, tolerance = 1e-8)
})

test_that("qpois handles log-scale probabilities correctly", {
    # Define a set of probabilities.
    probs <- c(0.1, 0.5, 0.9)

    # Compute quantiles using standard (non-log) and log-scale input.
    quantiles_standard <- qpois(n = 4, theta = 0.5, p = probs, lower.tail = TRUE, log.p = FALSE)
    quantiles_log      <- qpois(n = 4, theta = 0.5, p = log(probs), lower.tail = TRUE, log.p = TRUE)

    expect_equal(quantiles_standard, quantiles_log, tolerance = 1e-8)
})

test_that("qpois stops for probabilities outside [0,1]", {
    # Negative probability.
    expect_error(qpois(n = 4, theta = 0.5, p = c(-0.1, 0.5), lower.tail = TRUE, log.p = FALSE),
                 "Probabilities in 'p' must be between 0 and 1")

    # Probability greater than 1.
    expect_error(qpois(n = 4, theta = 0.5, p = c(0.1, 1.1), lower.tail = TRUE, log.p = FALSE),
                 "Probabilities in 'p' must be between 0 and 1")
})
