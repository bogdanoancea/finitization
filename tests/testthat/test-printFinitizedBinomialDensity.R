test_that("pdf0", {
  expect_equal(printFinitizedBinomialDensity(2,4,0),"1-4*p+6*p^2")
})


test_that("pdf1", {
    expect_equal(printFinitizedBinomialDensity(2,4,1),"-4*(-1+3*p)*p")
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
