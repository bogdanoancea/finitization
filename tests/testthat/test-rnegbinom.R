test_that("rnegbinom", {
    expect_length(rnegbinom(2,0.15,4,1001), 1001)
})

test_that("negbinom mean", {
    expect_equal(round(mean(rnegbinom(2, 0.15, 4, 1000000)), 1), 0.7)
})

test_that("negbinom var", {
    expect_equal(round(var(rnegbinom(2, 0.15, 4, 1000000)), 2), 0.83)
})
