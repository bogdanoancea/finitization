test_that("MFPS", {
  expect_equal(getPoissonMFPS(7), c(0,1))
})


test_that("MFPS2", {
    expect_equal(getPoissonMFPS(1), c(0,1))
})
