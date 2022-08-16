test_that("dnegbinom values", {
    y <- data.frame(val=c(0,1,2), prob=c(0.60553633, 0.08304498, 0.31141869))
    x <- dnegbinom(2,0.15,4)
    expect_equal(sum(round(x,8)!=round(y,8)), 0)
})

test_that("dnegbinom values 1", {
    y <- data.frame(val=c(0,1,2,3,4), prob=c(0.628783247, 0.269477400, 0.088480741, 0.005091307, 0.008167305 ))
    x <- dnegbinom(4,0.11,4)
    expect_equal( sum(round(x,8)!=round(y, 8)), 0)
})

test_that("dnegbinom values 2", {
    expect_true(is.null(dnegbinom(4,0.11,4, 5)))
})

test_that("dnegbinom values 3", {
    result <- data.frame(val = c(3), prob = c(0.1099125))
    expect_equal(round(dnegbinom(3,0.15,4,3), 7), result)
})
