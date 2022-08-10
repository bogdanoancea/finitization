test_that("binomial MFPS", {
  expect_equal(round(getBinomialMFPS(2,4), 7), c(0.0000000, 0.3333333) )
})
