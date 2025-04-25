test_that("rlog returns a vector of correct length and valid outcomes", {
    set.seed(123)
    values <- rlog(n = 2, theta = 0.15, no = 1001)

    # Check that the output has the expected length.
    expect_length(values, 1001)

    # Check that the output is numeric and its values are (or equal to) integers.
    expect_true(is.numeric(values))
    expect_true(all(values == as.integer(values)))

    # For finitization order n = 2, valid outcomes should be 0, 1, or 2.
    expect_true(all(values %in% 0:2))
})

test_that("rlog sample mean approximates the expected value", {
    set.seed(456)
    sample_size <- 1e6
    x <- rlog(n = 2, theta = 0.15, no = sample_size)
    # Calculate sample statistics
    sample_mean <- mean(x)
    sample_sd <- sd(x)
    sem <- sample_sd / sqrt(sample_size)  # standard error of the mean

    # Expected theoretical mean
    expected_mean <- 0.09

    # 95% confidence interval
    ci_low <- sample_mean - 1.96 * sem
    ci_high <- sample_mean + 1.96 * sem

    message <- paste0("Sample mean: ", round(sample_mean, 2),
                      "; 95% CI: [", round(ci_low, 2), ", ", round(ci_high, 2), "]")

    # Test that expected mean is within the CI of the sample mean
    expect_true(round(expected_mean,2) >= round(ci_low, 2) && round(expected_mean,2) <= round(ci_high, 2), info = message)
})

test_that("rlog sample variance approximates the expected value", {
    set.seed(789)
    sample_size <- 1e6
    x <- rlog(n = 2, theta = 0.15, no = sample_size)
    # Sample variance
    sample_var <- var(x)

    # Theoretical variance
    expected_var <- 0.10

    # Estimate standard error of the variance using asymptotic formula
    se_var <- sqrt(2 * expected_var^2 / (sample_size - 1))

    # Confidence interval (95%)
    ci_low <- sample_var - 1.96 * se_var
    ci_high <- sample_var + 1.96 * se_var

    msg <- paste0("Sample var = ", round(sample_var, 2),
                  "; Expected = ", round(expected_var, 2),
                  "; CI = [", round(ci_low, 2), ", ", round(ci_high, 2), "]")

    expect_true(round(expected_var,2) >= round(ci_low, 2) && round(expected_var,2) <= round(ci_high, 2), info = msg)})
