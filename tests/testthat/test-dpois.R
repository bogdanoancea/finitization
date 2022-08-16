test_that("dpois values", {
    y <-data.frame(val=c(0,1,2,3,4), prob=c(0.606770833,0.302083333,0.078125000,0.010416667,0.002604167))
    x <- dpois(4,0.5)
    expect_equal(sum(round(x,9)!=y), 0)
})


test_that("dpois values 2", {
    expect_true(is.null(dpois(7, 0.15, 9)))
})

test_that("dpois values 3", {
    result <- data.frame(val = 4, prob = 1.815513e-05)
    expect_equal(round(dpois(7, 0.15, 4), 11), result )
})
