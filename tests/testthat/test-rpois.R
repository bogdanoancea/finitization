test_that("rpois returns a vector of correct length and valid outcomes", {
    set.seed(123)
    result <- rpois(n = 4, theta = 0.5, no = 1000)

    # Check that the output has the expected length.
    expect_length(result, 1000)

    # Check that the output is integer-valued.
    # Depending on the implementation it might not be of class "integer",
    # so we check that each value is an integer.
    expect_true(all(result == as.integer(result)))

    # For a finitized Poisson distribution with n = 4,
    # valid outcomes should lie between 0 and 4.
    expect_true(all(result >= 0 & result <= 4))
})

test_that("rpois sample mean approximates the theoretical mean", {
    set.seed(12345)
    sample_size <- 1e6
    x <- rpois(n = 4, theta = 0.5, no = sample_size)

    # Calculate sample statistics
    sample_mean <- mean(x)
    sample_sd <- sd(x)
    sem <- sample_sd / sqrt(sample_size)  # standard error of the mean

    # Expected theoretical mean
    expected_mean <- 0.5

    # 95% confidence interval
    ci_low <- sample_mean - 1.96 * sem
    ci_high <- sample_mean + 1.96 * sem

    message <- paste0("Sample mean: ", round(sample_mean, 2),
                      "; 95% CI: [", round(ci_low, 2), ", ", round(ci_high, 2), "]")
    # Test that expected mean is within the CI of the sample mean
    expect_true(round(expected_mean,2) >= round(ci_low, 2) && round(expected_mean,2) <= round(ci_high, 2), info = message)


 })

test_that("rpois sample variance approximates the theoretical variance", {
    set.seed(54321)
    sample_size <- 1e6
    x <- rpois(n = 4, theta = 0.5, no = sample_size)

    # Sample variance
    sample_var <- var(x)

    # Theoretical variance
    expected_var <- 0.5

    # Estimate standard error of the variance using asymptotic formula
    se_var <- sqrt(2 * expected_var^2 / (sample_size - 1))

    # Confidence interval (95%)
    ci_low <- sample_var - 1.96 * se_var
    ci_high <- sample_var + 1.96 * se_var

    msg <- paste0("Sample var = ", round(sample_var, 2),
                  "; Expected = ", round(expected_var, 2),
                  "; CI = [", round(ci_low, 2), ", ", round(ci_high, 2), "]")

    expect_true(rround(expected_var,2) >= round(ci_low, 2) && round(expected_var,2) <= round(ci_high, 2), info = msg)
})
