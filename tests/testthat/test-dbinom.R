test_that("dbinom values", {
    y <- data.frame(val=c(0,1,2), prob=c(0.535, 0.330, 0.135))
    x <- dbinom(2,0.15,4)
    expect_equal(sum(round(x,3)!=y), 0)
})

test_that("dbinom values 1", {
    y <- data.frame(val=c(0,1,2,3,4), prob=c(0.52200625, 0.36847500, 0.09753750, 0.01147500, 0.00050625 ))
    x <- dbinom(4,0.15,4)
    expect_equal(sum(round(x,8)!=y), 0)
})

test_that("dbinom values 2", {
    expect_equal(dbinom(4,0.15,4, 5), 0)
})

test_that("dbinom values 3", {
    expect_equal(round(dbinom(3,0.15,4, 3), 4), 0.0135)
})
