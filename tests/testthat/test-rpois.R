test_that("length of the sequence", {
    expect_equal(length(rpois(4, 0.5, 1000)),
                 1000)
})

test_that("mean", {
    expect_equal(round(mean(rpois(4, 0.5, 1000000)), 2),
                 0.5)
})

test_that("var", {
    expect_equal(round(var(rpois(4, 0.5,  1000000)), 2),
                 0.5)
})
