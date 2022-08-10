test_that("pdf", {
  expect_equal(printFinitizedPoissonDensity(2,0), "1-theta+1/2*theta^2")
})

test_that("pdfL", {
    expect_equal(printFinitizedPoissonDensity(2,0, TRUE), "1-\\theta+\\frac{1}{2} \\theta^{2}")
})

test_that("pdf1", {
    expect_equal(printFinitizedPoissonDensity(2,1), "-(-1+theta)*theta")
})

test_that("pdfL1", {
    expect_equal(printFinitizedPoissonDensity(2,1, TRUE), "- \\theta {(-1+\\theta)}")
})


test_that("pdf2", {
    expect_equal(printFinitizedPoissonDensity(2,2), "1/2*theta^2")
})

test_that("pdfL2", {
    expect_equal(printFinitizedPoissonDensity(2,2, TRUE), "\\frac{1}{2}  \\theta^{2}")
})


test_that("pdf3", {
    expect_equal(printFinitizedPoissonDensity(2,3), "0")
})

test_that("pdfL3", {
    expect_equal(printFinitizedPoissonDensity(2,3, TRUE), "0")
})
