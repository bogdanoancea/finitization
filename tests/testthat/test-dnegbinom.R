test_that("dnegbinom returns correct densities for n = 2, q = 0.15, k = 4", {
    expected <- data.frame(
        val  = c(0, 1, 2),
        prob = c(0.60553633, 0.08304498, 0.31141869)
    )
    result <- dnegbinom(n = 2, q = 0.15, k = 4)

    # Check that the output is a data frame with proper column names.
    expect_s3_class(result, "data.frame")
    expect_equal(names(result), c("val", "prob"))

    # Check that the outcomes and probabilities match (within tolerance).
    expect_equal(result$val, expected$val)
    expect_equal(result$prob, expected$prob, tolerance = 1e-8)
})

test_that("dnegbinom returns correct densities for n = 4, q = 0.11, k = 4", {
    expected <- data.frame(
        val  = c(0, 1, 2, 3, 4),
        prob = c(0.628783247, 0.269477400, 0.088480741, 0.005091307, 0.008167305)
    )
    result <- dnegbinom(n = 4, q = 0.11, k = 4)

    expect_s3_class(result, "data.frame")
    expect_equal(result$val, expected$val)
    expect_equal(result$prob, expected$prob, tolerance = 1e-8)
})

test_that("dnegbinom returns NULL for invalid val input", {
    # For n = 4, valid outcomes are 0,1,2,3,4; providing 5 should be invalid.
    expect_null(dnegbinom(n = 4, q = 0.11, k = 4, val = 5))
})

test_that("dnegbinom returns correct density for a single value", {
    # For n = 3, q = 0.15, k = 4 and val = 3, the expected density is approximately 0.1099125.
    expected <- data.frame(
        val  = 3,
        prob = 0.1099125
    )
    result <- dnegbinom(n = 3, q = 0.15, k = 4, val = 3)

    expect_s3_class(result, "data.frame")
    expect_equal(result$val, expected$val)
    expect_equal(result$prob, expected$prob, tolerance = 1e-6)
})

test_that("dnegbinom returns log densities when log = TRUE", {
    # Compare the log densities with the log of the computed densities.
    result_normal <- dnegbinom(n = 2, q = 0.15, k = 4, log = FALSE)
    result_log    <- dnegbinom(n = 2, q = 0.15, k = 4, log = TRUE)

    expect_equal(result_log$prob, log(result_normal$prob), tolerance = 1e-8)
})
