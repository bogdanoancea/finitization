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
    sample_size <- 2e6
    res <- rnegbinom(n = 2, q = 0.15, k = 4, no = sample_size)

    sample_mean <- mean(res)
    sample_sd <- sd(res)
    sem <- sample_sd / sqrt(sample_size)  # Standard Error of the Mean

    expected_mean <- 0.70588

    expect_within_ci(
        observed = sample_mean,
        expected = expected_mean,
        se = sem,
        center = "expected",  # or "sample"
        level = 0.99,
        tolerance = 1e-3
    )
})


test_that("rnegbinom sample variance approximates expected variance", {
    set.seed(789)
    sample_size <- 2e6
    res <- rnegbinom(n = 2, q = 0.15, k = 4, no = sample_size)

    sample_var <- var(res)

    expected_var <- (4 * 0.15) / (0.85^2)  # ≈ 0.83034

    # Standard error of variance (using asymptotic formula)
    se_var <- sqrt(2 * expected_var^2 / (sample_size - 1))

    expect_within_ci(
        observed = sample_var,
        expected = expected_var,
        se = se_var,
        center = "expected",  # or "sample"
        level = 0.99,
        tolerance = 1e-3
    )
})


test_that("rnegbinom is consistent with dnegbinom (chi-square test)", {

    set.seed(123)

    n  <- 3         # finitization order
    N  <- 10        # size/shape parameter used by your finitized negbinom
    q  <- 0.05      # choose a value inside MFPS for this (n, N)
    no <- 200000

    ## --- Theoretical PMF ---
    pmf_df <- dnegbinom(n, q, N)     # data.frame(val, prob)
    probs  <- pmf_df$prob

    ## probability axioms
    expect_true(all(is.finite(probs)))
    expect_true(all(probs >= 0))
    expect_equal(sum(probs), 1, tolerance = 1e-12)

    ## --- Generate samples ---
    sample <- rnegbinom(n, q, N, no)

    ## observed counts on full support 0:n
    observed <- table(factor(sample, levels = 0:n))

    ## expected counts
    expected <- probs * no

    ## keep only categories with positive expectation
    keep <- expected > 0
    observed_k <- as.numeric(observed[keep])
    expected_k <- as.numeric(expected[keep])

    ## chi-square statistic
    chisq <- sum((observed_k - expected_k)^2 / expected_k)
    df <- length(expected_k) - 1

    pval <- pchisq(chisq, df = df, lower.tail = FALSE)

    expect_gt(pval, 1e-6)
})
