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

test_that("rlog is consistent with dlog (chi-square test)", {

    set.seed(123)

    n     <- 6        # finitization order
    theta <- 0.15     # choose a value inside MFPS for this n
    no    <- 200000   # large sample for stability

    ## --- Theoretical PMF ---
    pmf_df <- dlog(n, theta)       # data.frame(val, prob)
    probs  <- pmf_df$prob

    ## sanity checks (probability axioms)
    expect_true(all(is.finite(probs)))
    expect_true(all(probs >= 0))
    expect_equal(sum(probs), 1, tolerance = 1e-12)

    ## --- Generate samples ---
    sample <- rlog(n, theta, no)

    ## observed counts (force full support 0:n)
    observed <- table(factor(sample, levels = 0:n))

    ## expected counts
    expected <- probs * no

    ## avoid numerical issues if a tail prob is extremely tiny
    ## (chi-square assumes expected > 0)
    keep <- expected > 0
    observed_k <- as.numeric(observed[keep])
    expected_k <- as.numeric(expected[keep])

    ## chi-square statistic
    chisq <- sum((observed_k - expected_k)^2 / expected_k)

    ## degrees of freedom:
    ## K - 1  (no parameters estimated from data)
    df <- length(expected_k) - 1

    pval <- pchisq(chisq, df = df, lower.tail = FALSE)

    ## robust acceptance criterion (detects only real inconsistencies)
    expect_gt(pval, 1e-6)
})


