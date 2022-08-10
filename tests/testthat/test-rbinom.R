test_that("rbinom", {
  expect_length(rbinom(2,0.15,4,1001), 1001)
})

test_that("binom mean", {
    expect_equal(round(mean(rbinom(2, 0.15, 4, 1000000)), 2), 0.6)
})

test_that("binom var", {
    expect_equal(round(var(rbinom(2, 0.15, 4, 1000000)), 2), 0.51)
})
