test_that("log values", {
    y <- data.frame(val=c(0,1,2), prob=c(0.82487201, 0.04517918, 0.12994880))
    x <- dlog(2,0.4)
    expect_equal(sum(round(x,8)!=y), 0)
})

test_that("log values 1", {
    y <- data.frame(val=c(0,1,2,3), prob=c(0.836671870, 0.142741834, 0.002926472, 0.017659825 ))
    x <- dlog(3,0.3)
    expect_equal(sum(round(x,9)!=y), 0)
})

test_that("log values 2", {
    y <- data.frame(val=c(0,1,2,3, 4), prob=c(0.896407946, 0.089030447, 0.013085245, 0.000751355, 0.000725006 ))
    x <- dlog(4,0.2)
    expect_equal(sum(round(x,9)!=y), 0)
})

test_that("log values 3", {
    expect_true(is.null(dlog(4,0.2, 5)))
})
