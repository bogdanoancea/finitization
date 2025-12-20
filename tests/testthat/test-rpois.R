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

    # Generate sample
    x <- rpois(n = 4, theta = 0.5, no = sample_size)

    # Calculate sample statistics
    sample_mean <- mean(x)
    sample_sd <- sd(x)
    sem <- sample_sd / sqrt(sample_size)  # Standard Error of the Mean

    # Expected theoretical mean
    expected_mean <- 0.5

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

test_that("rpois sample variance approximates the theoretical variance", {
    set.seed(54321)
    sample_size <- 1e6

    # Generate sample
    x <- rpois(n = 6, theta = 0.5, no = sample_size)

    # Sample variance
    sample_var <- var(x)

    # Theoretical variance
    expected_var <- 0.5

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

test_that("rpois is consistent with dpois (chi-square test)", {

    set.seed(123)

    n      <- 6          # finitization order
    lambda <- 0.8        # inside MFPS for Poisson
    no     <- 200000     # large sample for stability

    ## --- Theoretical PMF ---
    pmf_df <- dpois(n, lambda)     # returns data.frame(val, prob)
    probs  <- pmf_df$prob

    ## sanity checks
    expect_equal(sum(probs), 1, tolerance = 1e-12)
    expect_true(all(probs >= 0))

    ## --- Generate samples ---
    sample <- rpois(n, lambda, no)

    ## observed counts (force full support 0:n)
    observed <- table(factor(sample, levels = 0:n))

    ## expected counts
    expected <- probs * no

    ## chi-square statistic
    chisq <- sum((observed - expected)^2 / expected)

    ## degrees of freedom:
    ## K - 1  (no parameters estimated from data)
    df <- (n + 1) - 1

    pval <- pchisq(chisq, df = df, lower.tail = FALSE)

    ## very weak but robust acceptance criterion
    expect_gt(pval, 1e-6)
})

