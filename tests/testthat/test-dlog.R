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




test_that("dlog returns correct densities for n = 2 and theta = 0.4", {
    expected <- data.frame(
        val  = c(0, 1, 2),
        prob = c(0.82487201, 0.04517918, 0.12994880)
    )

    result <- dlog(n = 2, theta = 0.4)

    # Check that the output is a data frame with the expected column names.
    expect_s3_class(result, "data.frame")
    expect_named(result, c("val", "prob"))

    # Compare the computed outcomes and densities with the expected ones.
    expect_equal(result$val, expected$val)
    expect_equal(result$prob, expected$prob)
})

test_that("dlog returns correct densities for n = 3 and theta = 0.3", {
    expected <- data.frame(
        val  = c(0, 1, 2, 3),
        prob = c(0.836671870, 0.142741834, 0.002926472, 0.017659825)
    )

    result <- dlog(n = 3, theta = 0.3)

    expect_s3_class(result, "data.frame")
    expect_named(result, c("val", "prob"))
    expect_equal(result$val, expected$val)
    expect_equal(result$prob, expected$prob)
})

test_that("dlog returns correct densities for n = 4 and theta = 0.2", {
    expected <- data.frame(
        val  = c(0, 1, 2, 3, 4),
        prob = c(0.896407946, 0.089030447, 0.013085245, 0.000751355, 0.000725006)
    )

    result <- dlog(n = 4, theta = 0.2)

    expect_s3_class(result, "data.frame")
    expect_named(result, c("val", "prob"))
    expect_equal(result$val, expected$val)
    expect_equal(result$prob, expected$prob)
})

test_that("dlog returns NULL for an invalid 'val' input", {
    # For n = 4, valid outcomes are 0, 1, 2, 3, 4.
    # Providing a value of 5 should yield NULL.
    expect_null(dlog(n = 4, theta = 0.2, val = 5))
})
