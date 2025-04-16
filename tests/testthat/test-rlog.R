test_that("rlog", {
    expect_length(rlog(2,0.15,1001), 1001)
})

test_that("log mean", {
    expect_equal(round(mean(rlog(2, 0.15, 1000000)), 2), 0.09)
})

test_that("log var", {
    expect_equal(round(var(rlog(2, 0.15, 1000000)), 2), 0.10)
})



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
    sample_size <- 1e6
    values <- rlog(n = 2, theta = 0.15, no = sample_size)
    sample_mean <- mean(values)

    # The test expectation: the sample mean is approximately 0.09.
    expect_equal(round(sample_mean, 2), 0.09)
})

test_that("rlog sample variance approximates the expected value", {
    set.seed(789)
    sample_size <- 1e6
    values <- rlog(n = 2, theta = 0.15, no = sample_size)
    sample_variance <- var(values)

    # The test expectation: the sample variance is approximately 0.10.
    expect_equal(round(sample_variance, 2), 0.10)
})
