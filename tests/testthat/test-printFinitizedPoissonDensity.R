test_that("Non-LaTeX output for val = 0 is correct", {
    expected <- c_printDensity(2, 0, list(), getPoissonType(), latex = FALSE)

    # Capture and trim the printed output.
    capture.output(out <- printFinitizedPoissonDensity(n = 2, val = 0, latex = FALSE))
    out <- trimws(out)

    expect_true(check_symbolic_equivalence(normalize_expr(out), normalize_expr(expected)),
                info = paste("Expressions not symbolically equivalent:\n", out, "\nvs\n", expected))

})

test_that("LaTeX output for val = 0 is correct", {
    r_latex_out <- capture.output(printFinitizedPoissonDensity(n = 2, val = 0, latex = TRUE))
    expect_gt(length(r_latex_out), 0)

    expected <- c_printDensity(2, 0, list(), getPoissonType(), latex = FALSE)

    # Capture and trim the printed output.
    capture.output(out <- printFinitizedPoissonDensity(n = 2, val = 0, latex = FALSE))
    out <- trimws(out)

    expect_true(check_symbolic_equivalence(normalize_expr(out), normalize_expr(expected)),
                info = paste("Expressions not symbolically equivalent:\n", out, "\nvs\n", expected))
})

test_that("Non-LaTeX output for val = 1 is correct", {
    expected <- c_printDensity(2, 1, list(), getPoissonType(), latex = FALSE)

    # Capture and trim the printed output.
    capture.output(out <- printFinitizedPoissonDensity(n = 2, val = 1, latex = FALSE))
    out <- trimws(out)

    expect_true(check_symbolic_equivalence(normalize_expr(out), normalize_expr(expected)),
                info = paste("Expressions not symbolically equivalent:\n", out, "\nvs\n", expected))})

test_that("LaTeX output for val = 1 is correct", {
    r_latex_out <- capture.output(printFinitizedPoissonDensity(n = 2, val = 1, latex = TRUE))
    expect_gt(length(r_latex_out), 0)

    expected <- c_printDensity(2, 1, list(), getPoissonType(), latex = FALSE)

    # Capture and trim the printed output.
    capture.output(out <- printFinitizedPoissonDensity(n = 2, val = 1, latex = FALSE))
    out <- trimws(out)

    expect_true(check_symbolic_equivalence(normalize_expr(out), normalize_expr(expected)),
                info = paste("Expressions not symbolically equivalent:\n", out, "\nvs\n", expected))})

test_that("Non-LaTeX output for val = 2 is correct", {
    expected <- c_printDensity(2, 2, list(), getPoissonType(), latex = FALSE)

    # Capture and trim the printed output.
    capture.output(out <- printFinitizedPoissonDensity(n = 2, val = 2, latex = FALSE))
    out <- trimws(out)

    expect_true(check_symbolic_equivalence(normalize_expr(out), normalize_expr(expected)),
                info = paste("Expressions not symbolically equivalent:\n", out, "\nvs\n", expected))
})

test_that("LaTeX output for val = 2 is correct", {
    r_latex_out <- capture.output(printFinitizedPoissonDensity(n = 2, val = 0, latex = TRUE))
    expect_gt(length(r_latex_out), 0)

    expected <- c_printDensity(2, 0, list(), getPoissonType(), latex = FALSE)

    # Capture and trim the printed output.
    capture.output(out <- printFinitizedPoissonDensity(n = 2, val = 0, latex = FALSE))
    out <- trimws(out)

    expect_true(check_symbolic_equivalence(normalize_expr(out), normalize_expr(expected)),
                info = paste("Expressions not symbolically equivalent:\n", out, "\nvs\n", expected))
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

