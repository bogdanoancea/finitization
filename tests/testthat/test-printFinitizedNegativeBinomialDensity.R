
test_that("printFinitizedNegativeBinomialDensity produces correct non-LaTeX output for a valid value (val = 2)", {
    expected <- c_printDensity(2, 2, list("k" = 4), getNegativeBinomialType(), latex = FALSE)

    # Capture and trim the printed output.
    capture.output(out <- printFinitizedNegativeBinomialDensity(n = 2, k = 4, val = 2, latex = FALSE))
    out <- trimws(out)

    expect_true(check_symbolic_equivalence(normalize_expr(out), normalize_expr(expected)),
                info = paste("Expressions not symbolically equivalent:\n", out, "\nvs\n", expected))
})

test_that("printFinitizedNegativeBinomialDensity produces correct non-LaTeX output for a different valid value (val = 1)", {
    expected <- c_printDensity(2, 1, list("k" = 4), getNegativeBinomialType(), latex = FALSE)

    capture.output(out <- printFinitizedNegativeBinomialDensity(n = 2, k = 4, val = 1, latex = FALSE))
    out <- trimws(out)
    expect_true(check_symbolic_equivalence(normalize_expr(out), normalize_expr(expected)),
                info = paste("Expressions not symbolically equivalent:\n", out, "\nvs\n", expected))

})

test_that("Latex output is generated and symbolic equivalence holds", {
    r_latex_out <- capture.output(printFinitizedNegativeBinomialDensity(n = 2, k = 4, val = 2, latex = TRUE))
    expect_gt(length(r_latex_out), 0)

    expected <- c_printDensity(2, 2, list("k" = 4), getNegativeBinomialType(), latex = FALSE)

    # Capture and trim the printed output.
    capture.output(out <- printFinitizedNegativeBinomialDensity(n = 2, k = 4, val = 2, latex = FALSE))
    out <- trimws(out)

    expect_true(check_symbolic_equivalence(normalize_expr(out), normalize_expr(expected)),
                info = paste("Expressions not symbolically equivalent:\n", out, "\nvs\n", expected))

})

test_that("printFinitizedNegativeBinomialDensity returns no output and NULL for invalid 'val'", {
    out <- capture.output(res <- suppressMessages(printFinitizedNegativeBinomialDensity(n = 2, k = 4, val = 3, latex = TRUE)))
    expect_equal(length(out), 0)
    expect_null(suppressMessages(printFinitizedNegativeBinomialDensity(n = 2, k = 4, val = 3, latex = TRUE)))
    out <- capture.output(res <- suppressMessages(printFinitizedNegativeBinomialDensity(n = 2, k = 4, val = 3, latex = FALSE)))
    expect_equal(length(out), 0)
    expect_null(suppressMessages(printFinitizedNegativeBinomialDensity(n = 2, k = 4, val = 3, latex = FALSE)))
})
