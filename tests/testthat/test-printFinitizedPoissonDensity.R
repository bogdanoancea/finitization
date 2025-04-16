test_that("Non-LaTeX output for val = 0 is correct", {
    # Capture the output for val = 0 (non-LaTeX mode)
    capture.output(out <- printFinitizedPoissonDensity(2, val = 0))
    out <- trimws(out)

    # Acceptable equivalent strings
    expected <- c("1-theta+1/2*theta^2", "1+1/2*theta^2-theta")

    expect_true(out == expected[1] || out == expected[2])
})

test_that("LaTeX output for val = 0 is correct", {
    capture.output(out <- printFinitizedPoissonDensity(2, val = 0, latex = TRUE))
    out <- trimws(out)

    expected <- c("1-\\theta+\\frac{1}{2} \\theta^{2}", "1+\\frac{1}{2} \\theta^{2}-\\theta")

    expect_true(out %in% expected)
})

test_that("Non-LaTeX output for val = 1 is correct", {
    capture.output(out <- printFinitizedPoissonDensity(2, val = 1))
    out <- trimws(out)

    expected <- c("-(-1+theta)*theta", "-theta*(-1+theta)")

    expect_true(out %in% expected)
})

test_that("LaTeX output for val = 1 is correct", {
    capture.output(out <- printFinitizedPoissonDensity(2, val = 1, latex = TRUE))
    out <- trimws(out)

    expected <- c("- {(-1+\\theta)} \\theta", "- \\theta {(-1+\\theta)}")

    expect_true(out %in% expected)
})

test_that("Non-LaTeX output for val = 2 is correct", {
    capture.output(out <- printFinitizedPoissonDensity(2, val = 2))
    out <- trimws(out)

    expect_equal(out, "1/2*theta^2")
})

test_that("LaTeX output for val = 2 is correct", {
    capture.output(out <- printFinitizedPoissonDensity(2, val = 2, latex = TRUE))
    out <- trimws(out)

    expect_equal(out, "\\frac{1}{2}  \\theta^{2}")
})

test_that("Output and return value for an invalid 'val' (nonexistent index) are handled correctly (non-LaTeX)", {
    capture.output(out <- printFinitizedPoissonDensity(2, val = 3))

    # Expect no printed output.
    expect_equal(length(out), 0)
    # And the function should return NULL.
    expect_null(printFinitizedPoissonDensity(2, val = 3))
})

test_that("Output and return value for an invalid 'val' (nonexistent index) are handled correctly (LaTeX)", {
    capture.output(out <- printFinitizedPoissonDensity(2, val = 3, latex = TRUE))

    expect_equal(length(out), 0)
    expect_null(printFinitizedPoissonDensity(2, val = 3, latex = TRUE))
})

