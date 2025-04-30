test_that("pdf2", {
    expected <- c_printDensity(2, 0, list(), getLogarithmicType(), latex = FALSE)

    # Capture and trim the printed output.
    capture.output(out <- printFinitizedLogarithmicDensity(n = 2, val = 0, latex = FALSE))
    out <- trimws(out)

    expect_true(check_symbolic_equivalence(normalize_expr(out), normalize_expr(expected)),
                info = paste("Expressions not symbolically equivalent:\n", out, "\nvs\n", expected))

})



test_that("pdf3", {
     capture.output(b <- suppressMessages(printFinitizedLogarithmicDensity(2,3)))
     expect_true(is.null(b))
})


test_that("pdf1", {
    expected <- c_printDensity(2, 1, list(), getLogarithmicType(), latex = FALSE)

    # Capture and trim the printed output.
    capture.output(out <- printFinitizedLogarithmicDensity(n = 2, val = 1, latex = FALSE))
    out <- trimws(out)

    expect_true(check_symbolic_equivalence(normalize_expr(out), normalize_expr(expected)),
                info = paste("Expressions not symbolically equivalent:\n", out, "\nvs\n", expected))


})

test_that("pdf0", {
    expected <- c_printDensity(2, 0, list(), getLogarithmicType(), latex = FALSE)

    # Capture and trim the printed output.
    capture.output(out <- printFinitizedLogarithmicDensity(n = 2, val = 0, latex = FALSE))
    out <- trimws(out)

    expect_true(check_symbolic_equivalence(normalize_expr(out), normalize_expr(expected)),
                info = paste("Expressions not symbolically equivalent:\n", out, "\nvs\n", expected))


})


test_that("Latex output is generated and symbolic equivalence holds", {
    r_latex_out <- capture.output(printFinitizedLogarithmicDensity(n = 2, val = 2, latex = TRUE))
    expect_gt(length(r_latex_out), 0)

    expected <- c_printDensity(2, 2, list(), getLogarithmicType(), latex = FALSE)

    # Capture and trim the printed output.
    capture.output(out <- printFinitizedLogarithmicDensity(n = 2, val = 2, latex = FALSE))
    out <- trimws(out)

    expect_true(check_symbolic_equivalence(normalize_expr(out), normalize_expr(expected)),
                info = paste("Expressions not symbolically equivalent:\n", out, "\nvs\n", expected))
})


test_that("pdf3L", {
    out <- capture.output(res <- suppressMessages(printFinitizedLogarithmicDensity(n = 2, val = 3, latex = TRUE)))
    expect_equal(length(out), 0)
    expect_null(suppressMessages(printFinitizedLogarithmicDensity(n = 2, val = 3, latex = TRUE)))
    out <- capture.output(res <- suppressMessages(printFinitizedLogarithmicDensity(n = 2, val = 3, latex = FALSE)))
    expect_equal(length(out), 0)
    expect_null(suppressMessages(printFinitizedLogarithmicDensity(n = 2, val = 3, latex = FALSE)))

})


