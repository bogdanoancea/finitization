test_that("rlog", {
    expect_length(rlog(2,0.15,1001), 1001)
})

test_that("log mean", {
    expect_equal(round(mean(rlog(2, 0.15, 1000000)), 2), 0.09)
})

test_that("log var", {
    expect_equal(round(var(rlog(2, 0.15, 1000000)), 2), 0.10)
})
