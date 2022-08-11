test_that("logarithmic MFPS", {
    expect_equal(round(getLogarithmicMFPS(2), 7), c(0.0000000, 0.4404231) )
})

test_that("logarithmic MFPS 1", {
    expect_equal(round(getLogarithmicMFPS(3), 7), c(0.0000081, 0.3115801) )
})

test_that("logarithmic MFPS 2", {
    expect_equal(round(getLogarithmicMFPS(4), 7), c(0.0001775, 0.2397019) )
})


test_that("logarithmic MFPS 3", {
    expect_equal(round(getLogarithmicMFPS(7), 3), c(0.008, 0.140) )
})
