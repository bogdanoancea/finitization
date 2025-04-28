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
    sample_size <- 2e6

    # Generate sample
    x <- rlog(n = 2, theta = 0.15, no = sample_size)

    # Calculate sample statistics
    sample_mean <- mean(x)
    sample_sd <- sd(x)
    sem <- sample_sd / sqrt(sample_size)  # Standard Error of the Mean

    # Expected theoretical mean
    expected_mean <- 0.09

    # Use helper function to check
    expect_within_ci(
        observed = sample_mean,
        expected = expected_mean,
        se = sem,
        center = "expected",
        level = 0.99,
        tolerance = 1e-2
    )
})

test_that("rlog sample variance approximates the expected value", {
    set.seed(789)
    sample_size <- 2e6

    # Generate sample
    x <- rlog(n = 2, theta = 0.15, no = sample_size)

    # Sample variance
    sample_var <- var(x)

    # Theoretical variance
    expected_var <- 0.10

    # Estimate standard error of the variance using asymptotic formula
    se_var <- sqrt(2 * expected_var^2 / (sample_size - 1))

    # Use helper function to check
    expect_within_ci(
        observed = sample_var,
        expected = expected_var,
        se = se_var,
        center = "expected",
        level = 0.99,
        tolerance = 1e-2
    )
})

