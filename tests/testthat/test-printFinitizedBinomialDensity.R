test_that("pdf0", {
    capture.output(b <- printFinitizedBinomialDensity(2,4,0))
  expect_true(b == "1-4*p+6*p^2" || b == "1+6*p^2-4*p")
})


test_that("pdf1", {
  capture.output(b <- printFinitizedBinomialDensity(2,4,1))
    expect_true( b == "-4*(-1+3*p)*p" || b == "-4*p*(-1+3*p)" )
})


test_that("pdf2", {
  capture.output(expect_equal(printFinitizedBinomialDensity(2,4,2),"6*p^2"))
})

test_that("pdf3", {
  capture.output(expect_equal(printFinitizedBinomialDensity(2,4,3), NULL))
})


test_that("pdf2L", {
  capture.output(expect_equal(printFinitizedBinomialDensity(2,4,2, TRUE),"6  p^{2}"))
})

test_that("pdf3L", {
  capture.output(expect_equal(printFinitizedBinomialDensity(2,4,3, TRUE), NULL))
})

