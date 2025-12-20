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
    sem <- sample_sd / sqrt(sample_size)  # Standard Error of the Mean

    # Expected theoretical mean
    expected_mean <- 0.6

    # Use helper function to check
    expect_within_ci(
        observed = sample_mean,
        expected = expected_mean,
        se = sem,
        center = "expected",
        level = 0.99,
        tolerance = 1e-3
    )
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

    # Use helper function to check
    expect_within_ci(
        observed = sample_var,
        expected = expected_var,
        se = se_var,
        center = "expected",
        level = 0.99,
        tolerance = 1e-3
    )
})

test_that("binomial GOF: observed frequencies match dbinom() probabilities", {

    n  <- 5
    N  <- 10
    p  <- 0.10
    no <- 200000

    # draw samples (your finitization API)
    sample <- rbinom(n = n, p = p, N = N, no = no)

    # observed counts on full support 0..n (keep zeros)
    observed <- as.integer(table(factor(sample, levels = 0:n)))

    # expected probabilities from package dbinom(): returns a data.frame(val, prob)
    df <- dbinom(n = n, p = p, N = N, val = 0:n)
    expected_probs <- as.numeric(df$prob)  # numeric vector length n+1

    # expected counts
    expected <- expected_probs * no

    # sanity checks
    expect_equal(length(expected_probs), n + 1)
    expect_true(all(is.finite(expected_probs)))
    expect_true(all(expected_probs >= 0))
    expect_lt(abs(sum(expected_probs) - 1), 1e-12)

    # Pearson chi-square GOF (only where expected is non-negligible)
    keep <- expected > 5
    chisq <- sum((observed[keep] - expected[keep])^2 / expected[keep])

    dfree <- sum(keep) - 1  # no parameters estimated here
    pval <- stats::pchisq(chisq, df = dfree, lower.tail = FALSE)

    # with large no, this should be comfortably non-significant if RNG matches pmf
    expect_gt(pval, 1e-6)
})
