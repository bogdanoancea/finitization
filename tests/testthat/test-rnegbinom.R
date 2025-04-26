test_that("rnegbinom returns a vector of correct length and valid outcomes", {
    set.seed(123)
    # For n = 2, the support is assumed to be 0, 1, and 2.
    res <- rnegbinom(n = 2, q = 0.15, k = 4, no = 1001)

    # Check that the output has the expected length.
    expect_length(res, 1001)

    # Check that the output is numeric.
    expect_true(is.numeric(res))

    # Check that each outcome is an integer (or equivalent to an integer).
    expect_true(all(res == floor(res)))

    # For n = 2, valid outcomes are 0, 1, or 2.
    expect_true(all(res %in% 0:2))
})


test_that("rnegbinom sample mean approximates expected mean", {
    set.seed(456)
    sample_size <- 1e6

    # Generate sample
    res <- rnegbinom(n = 2, q = 0.15, k = 4, no = sample_size)

    # Early sanity checks
    expect_type(res, "integer", info = "rnegbinom should return integer samples")
    expect_length(res, sample_size, info = "rnegbinom sample size mismatch")

    # Calculate sample statistics
    sample_mean <- mean(res)
    sample_sd <- sd(res)
    sem <- sample_sd / sqrt(sample_size)  # Standard Error of the Mean

    # Expected theoretical mean
    expected_mean <- 0.70588

    # Calculate 95% confidence interval
    ci_low <- sample_mean - 1.96 * sem
    ci_high <- sample_mean + 1.96 * sem

    # Allow a **slight tolerance** for floating point noise
    tolerance <- 1e-3

    message <- paste0(
        "Sample mean: ", round(sample_mean, 5),
        "; Expected mean: ", expected_mean,
        "; 95% CI: [", round(ci_low, 5), ", ", round(ci_high, 5), "]"
    )

    # Test that expected mean falls inside the 95% confidence interval (+ small numerical tolerance)
    expect_true(
        expected_mean >= ci_low - tolerance && expected_mean <= ci_high + tolerance,
        info = message
    )
})

test_that("rnegbinom sample variance approximates expected variance", {
    set.seed(789)
    sample_size <- 1e6
    res <- rnegbinom(n = 2, q = 0.15, k = 4, no = sample_size)

    # Sample variance
    sample_var <- var(res)

    # Theoretical variance
    expected_var <- (4 * 0.15) / (0.85^2)  # ≈ 0.83034

    # Estimate standard error of the variance using asymptotic formula
    # se_var ≈ sqrt(2 * variance^2 / (n - 1))
    se_var <- sqrt(2 * expected_var^2 / (sample_size - 1))

    # Confidence interval (95%)
    ci_low <- sample_var - 1.96 * se_var
    ci_high <- sample_var + 1.96 * se_var

    msg <- paste0("Sample var = ", round(sample_var, 2),
                  "; Expected = ", round(expected_var, 2),
                  "; CI = [", round(ci_low, 2), ", ", round(ci_high, 2), "]")

    expect_true(round(expected_var,2) >= round(ci_low, 2) && round(expected_var,2) <= round(ci_high, 2), info = msg)
})
