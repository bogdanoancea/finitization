

test_that("rbinom returns a vector of correct length and valid outcomes", {
    set.seed(123)
    x <- rbinom(n = 2, p = 0.15, N = 4, no = 1001)

    # Check that the output is of the expected length.
    expect_length(x, 1001)

    # Check that the vector is numeric/integer.
    # Depending on your implementation, rbinom may return an integer vector or a numeric vector
    # with integer values.
    expect_true(is.integer(x) || all(x == as.integer(x)))

    # For finitization order n = 2, outcomes should be in the set {0, 1, 2}.
    expect_true(all(x %in% 0:2))
})

test_that("rbinom sample mean approximates the theoretical mean", {
    set.seed(456)
    # Generate a large sample to check mean.
    x <- rbinom(n = 2, p = 0.15, N = 4, no = 1e6)
    sample_mean <- mean(x)

    # Based on previous tests and expectations, the theoretical mean should be about 0.6.
    # Adjust tolerance as needed.
    expect_equal(sample_mean, 0.6, tolerance = 0.01)
})

test_that("rbinom sample variance approximates the theoretical variance", {
    set.seed(789)
    # Generate a large sample to check variance.
    x <- rbinom(n = 2, p = 0.15, N = 4, no = 1e6)
    sample_var <- var(x)

    # Expect the variance to be approximately 0.51 based on prior expectations.
    expect_equal(sample_var, 0.51, tolerance = 0.02)
})
