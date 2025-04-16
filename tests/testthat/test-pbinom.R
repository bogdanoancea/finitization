

test_that("pbinom returns a numeric vector when 'val' is provided", {
    # For finitized Binomial with n = 4, p = 0.5, N = 4, specify some values.
    vals <- c(0, 2, 4)
    result <- pbinom(n = 4, p = 0.5, N = 4, val = vals, lower.tail = TRUE, log.p = FALSE)

    expect_type(result, "double")
    expect_equal(length(result), length(vals))

    # Check that each returned probability is between 0 and 1.
    expect_true(all(result >= 0 & result <= 1))
})

test_that("pbinom returns a data frame when 'val' is NULL", {
    df <- pbinom(n = 4, p = 0.5, N = 4, val = NULL, lower.tail = TRUE, log.p = FALSE)

    expect_s3_class(df, "data.frame")
    # The number of rows should be n+1 (for outcomes 0 through n).
    expect_equal(nrow(df), 4 + 1)
    expect_true(all(c("val", "cdf") %in% names(df)))

    # Check that the CDF for the maximum value equals 1.
    expect_equal(df$cdf[nrow(df)], 1)
})

test_that("pbinom handles lower.tail correctly", {
    # Compute lower-tail probabilities and upper-tail (using lower.tail = FALSE).
    vals <- c(0, 1, 2, 3, 4)
    lower <- pbinom(n = 4, p = 0.5, N = 4, val = vals, lower.tail = TRUE, log.p = FALSE)
    upper <- pbinom(n = 4, p = 0.5, N = 4, val = vals, lower.tail = FALSE, log.p = FALSE)

    # For each outcome x, the two should sum to 1.
    expect_equal(lower + upper, rep(1, length(lower)))
})

test_that("pbinom returns log-scale probabilities when log.p is TRUE", {
    # Compute the standard and log probabilities.
    vals <- c(0, 2, 4)
    normal_probs <- pbinom(n = 4, p = 0.5, N = 4, val = vals, lower.tail = TRUE, log.p = FALSE)
    log_probs <- pbinom(n = 4, p = 0.5, N = 4, val = vals, lower.tail = TRUE, log.p = TRUE)

    # The log of the normal probabilities should equal the log probabilities.
    expect_equal(log(normal_probs), log_probs)
})
