test_that("pdf0", {
    b <- printFinitizedBinomialDensity(2,4,0)
  expect_true(b == "1-4*p+6*p^2" || b == "1+6*p^2-4*p")
})


test_that("pdf1", {
    b <- printFinitizedBinomialDensity(2,4,1)
    expect_true( b == "-4*(-1+3*p)*p" || b == "-4*p*(-1+3*p)" )
})


test_that("pdf2", {
    expect_equal(printFinitizedBinomialDensity(2,4,2),"6*p^2")
})

test_that("pdf3", {
    expect_equal(printFinitizedBinomialDensity(2,4,3),"0")
})


test_that("pdf2L", {
    expect_equal(printFinitizedBinomialDensity(2,4,2, TRUE),"6  p^{2}")
})

test_that("pdf3L", {
    expect_equal(printFinitizedBinomialDensity(2,4,3, TRUE),"0")
})
