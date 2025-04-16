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
    result <- rpois(n = 4, theta = 0.5, no = sample_size)
    sample_mean <- mean(result)

    # For theta = 0.5, the theoretical mean is expected to be approximately 0.5.
    # Here we allow a small tolerance for sampling variability.
    expect_equal(sample_mean, 0.5, tolerance = 0.01)
})

test_that("rpois sample variance approximates the theoretical variance", {
    set.seed(54321)
    sample_size <- 1e6
    result <- rpois(n = 4, theta = 0.5, no = sample_size)
    sample_var <- var(result)

    # For theta = 0.5, the theoretical variance is expected to be approximately 0.5.
    expect_equal(sample_var, 0.5, tolerance = 0.02)
})
