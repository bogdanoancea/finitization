test_that("negative binomial MFPS", {
    expect_equal(round(getNegativeBinomialMFPS(2,4), 7), c(0.0000000, 0.1666667) )
})

test_that("negative binomial MFPS 1", {
    expect_equal(round(getNegativeBinomialMFPS(3,4), 7), c(0.0000000, 0.1428571) )
})
