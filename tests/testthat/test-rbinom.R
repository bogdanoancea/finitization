test_that("rbinom returns a vector of correct length and valid outcomes", {
    set.seed(123)
    x <- rbinom(n = 2, p = 0.15, N = 4, no = 1001)

    # Check that the output is of the expected length.
    expect_length(x, 1001)

    # Check that the output is numeric.
    expect_true(is.numeric(x))

    # Check that each outcome is an integer (or equivalent to an integer).
    expect_true(all(x == floor(x)))
    # For finitization order n = 2, outcomes should be in the set {0, 1, 2}.
    expect_true(all(x %in% 0:2))
})

test_that("rbinom sample mean approximates the theoretical mean", {
    set.seed(456)
    sample_size <- 1e6

    # Generate sample
    x <- rbinom(n = 2, p = 0.15, N = 4, no = sample_size)

    # Calculate sample statistics
    sample_mean <- mean(x)
    sample_sd <- sd(x)
    sem <- sample_sd / sqrt(sample_size)  # standard error of the mean

    # Expected theoretical mean
    expected_mean <- 0.6

    # 95% confidence interval
    ci_low <- sample_mean - 1.96 * sem
    ci_high <- sample_mean + 1.96 * sem

    message <- paste0("Sample mean: ", round(sample_mean, 2),
                      "; 95% CI: [", round(ci_low, 2), ", ", round(ci_high, 2), "]")

    # Test that expected mean is within the CI of the sample mean
    expect_true(expected_mean >= round(ci_low, 2) && expected_mean <= round(ci_high, 2), info = message)
})

test_that("rbinom sample variance approximates the theoretical variance", {
    set.seed(789)
    sample_size <- 1e6
    x <- rbinom(n = 2, p = 0.15, N = 4, no = sample_size)

    # Sample variance
    sample_var <- var(x)

    # Theoretical variance
    expected_var <- 0.51

    # Estimate standard error of the variance using asymptotic formula
    se_var <- sqrt(2 * expected_var^2 / (sample_size - 1))

    # Confidence interval (95%)
    ci_low <- sample_var - 1.96 * se_var
    ci_high <- sample_var + 1.96 * se_var

    msg <- paste0("Sample var = ", round(sample_var, 2),
                  "; Expected = ", round(expected_var, 2),
                  "; CI = [", round(ci_low, 2), ", ", round(ci_high, 2), "]")

    expect_true(expected_var >= round(ci_low, 2) && expected_var <= round(ci_high, 2), info = msg)
})
