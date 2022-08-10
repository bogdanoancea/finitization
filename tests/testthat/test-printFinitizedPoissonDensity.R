test_that("pdf", {
    b <- printFinitizedPoissonDensity(2,0)
  expect_true(b == "1-theta+1/2*theta^2" || b == "1+1/2*theta^2-theta")
})

test_that("pdfL", {
    b <- printFinitizedPoissonDensity(2,0, TRUE)
    expect_true( b == "1-\\theta+\\frac{1}{2} \\theta^{2}" || b == "1+\\frac{1}{2} \\theta^{2}-\\theta" )
})

test_that("pdf1", {
    b <- printFinitizedPoissonDensity(2,1)
    expect_true( b == "-(-1+theta)*theta" || b == "-theta*(-1+theta)")
})

test_that("pdfL1", {
    b <- printFinitizedPoissonDensity(2,1, TRUE)
    expect_true( b == "- {(-1+\\theta)} \\theta" || b =="- \\theta {(-1+\\theta)}")
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
