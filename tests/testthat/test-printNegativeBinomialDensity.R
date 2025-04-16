test_that("printFinitizedNegativeBinomialDensity produces correct non-LaTeX output for a valid value (val = 2)", {
    # For n = 2, k = 4 and val = 2, acceptable non-LaTeX outputs include one of:
    valid_outputs <- c("10*(-1+q)^(-2)*q^2", "10*q^2*(-1+q)^(-2)")

    # Capture and trim the printed output.
    capture.output(out <- printFinitizedNegativeBinomialDensity(n = 2, k = 4, val = 2, latex = FALSE))
    out <- trimws(out)

    expect_true(out %in% valid_outputs)
})

test_that("printFinitizedNegativeBinomialDensity produces correct non-LaTeX output for a different valid value (val = 1)", {
    # For n = 2, k = 4 and val = 1, acceptable outputs are variants of:
    valid_outputs <- c(
        "-4*q*((-1+q)^(-1)+5*q*(-1+q)^(-2))",
        "-4*q*(5*(-1+q)^(-2)*q+(-1+q)^(-1))",
        "-4*((-1+q)^(-1)+5*(-1+q)^(-2)*q)*q",
        "-4*((-1+q)^(-1)+5*q*(-1+q)^(-2))*q",
        "-4*q*(5*q*(-1+q)^(-2)+(-1+q)^(-1))",
        "-4*(5*(-1+q)^(-2)*q+(-1+q)^(-1))*q",
        "-4*q*((-1+q)^(-1)+5*(-1+q)^(-2)*q)",
        "-4*(5*q*(-1+q)^(-2)+(-1+q)^(-1))*q"
    )

    capture.output(out <- printFinitizedNegativeBinomialDensity(n = 2, k = 4, val = 1, latex = FALSE))
    out <- trimws(out)

    expect_true(out %in% valid_outputs)
})

test_that("printFinitizedNegativeBinomialDensity returns no output and NULL for invalid 'val'", {
    # For n = 2, valid values are 0, 1, or 2. Supplying val = 3 should result in no printed output and NULL.
    capture.output(out <- printFinitizedNegativeBinomialDensity(n = 2, k = 4, val = 3, latex = FALSE))
    expect_equal(length(out), 0)
    expect_null(printFinitizedNegativeBinomialDensity(n = 2, k = 4, val = 3, latex = FALSE))
})

test_that("printFinitizedNegativeBinomialDensity produces correct LaTeX output for a valid value (val = 2)", {
    # For LaTeX mode, suppose the expected representation for n = 2, k = 4, val = 2 is one of:
    valid_outputs_latex <- c("\\frac{10q^{2}}{(-1+q)^{2}}", "\\frac{10q^2}{(-1+q)^2}", "10 \\frac{q^{2}}{{(-1+q)}^{2}}")

    capture.output(out <- printFinitizedNegativeBinomialDensity(n = 2, k = 4, val = 2, latex = TRUE))
    out <- trimws(out)

    expect_true(out %in% valid_outputs_latex)
})

test_that("printFinitizedNegativeBinomialDensity returns no output and NULL in LaTeX mode for invalid 'val'", {
    out <- capture.output(res <- printFinitizedNegativeBinomialDensity(n = 2, k = 4, val = 3, latex = TRUE))
    expect_equal(length(out), 0)
    expect_null(printFinitizedNegativeBinomialDensity(n = 2, k = 4, val = 3, latex = TRUE))
})
