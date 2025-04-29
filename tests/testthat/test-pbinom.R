test_that("pbinom returns a numeric vector when 'val' is provided", {
    vals <- c(0, 2, 4)
    result <- pbinom(n = 4, p = 0.5, N = 4, val = vals, lower.tail = TRUE, log.p = FALSE)

    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 3, info = "Expected 3 rows for outcomes 0, 2, 4 (n = 4)")
    expect_equal(length(result$cdf), length(vals), info = "Length of result should match number of vals provided")
    expect_true(all(c("val", "cdf") %in% names(result)), info = "Data frame should contain 'val' and 'cdf' columns")

    expect_true(all(result$cdf >= 0 & result$cdf <= 1),
                info = paste("Some probabilities are outside [0, 1]:", toString(result)))
})

test_that("pbinom returns a data frame when 'val' is NULL", {
    df <- pbinom(n = 4, p = 0.5, N = 4, val = NULL, lower.tail = TRUE, log.p = FALSE)

    expect_s3_class(df, "data.frame")
    expect_equal(nrow(df), 5, info = "Expected 5 rows for outcomes 0 to 4 (n = 4)")
    expect_true(all(c("val", "cdf") %in% names(df)), info = "Data frame should contain 'val' and 'cdf' columns")
    expect_equal(df$cdf[nrow(df)], 1, info = "CDF of the last row should be 1")
})

test_that("pbinom handles lower.tail correctly", {
    vals <- c(0, 1, 2, 3, 4)
    lower <- pbinom(n = 4, p = 0.5, N = 4, val = vals, lower.tail = TRUE, log.p = FALSE)
    upper <- pbinom(n = 4, p = 0.5, N = 4, val = vals, lower.tail = FALSE, log.p = FALSE)

    expect_equal(lower$cdf + upper$cdf, rep(1, nrow(lower)),
                 info = paste("Lower + Upper tail probabilities do not sum to 1:",
                              "\nLower:", toString(lower$cdf),
                              "\nUpper:", toString(upper$cdf),
                              "\nSum:  ", toString(lower$cdf + upper$cdf)))
})

test_that("pbinom returns log-scale probabilities when log.p is TRUE", {
    vals <- c(0, 2, 4)
    normal_probs <- pbinom(n = 4, p = 0.5, N = 4, val = vals, lower.tail = TRUE, log.p = FALSE)
    log_probs <- pbinom(n = 4, p = 0.5, N = 4, val = vals, lower.tail = TRUE, log.p = TRUE)

    expect_equal(log(normal_probs$cdf), log_probs$cdf,
                 info = "Log of regular probabilities should match log.p = TRUE output")
})
