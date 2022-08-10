test_that("pdf0", {
    b <- printFinitizedNegativeBinomialDensity(2,4,2)
    expect_true(b == "10*(-1+q)^(-2)*q^2" || b == "10*q^2*(-1+q)^(-2)")
})


test_that("pdf1", {
    b <- printFinitizedNegativeBinomialDensity(2,4,1)
    expect_true(b == "-4*q*((-1+q)^(-1)+5*q*(-1+q)^(-2))" || b == "-4*q*(5*(-1+q)^(-2)*q+(-1+q)^(-1))" || b == "-4*((-1+q)^(-1)+5*(-1+q)^(-2)*q)*q" || b == "-4*((-1+q)^(-1)+5*q*(-1+q)^(-2))*q"
                || b =="-4*q*(5*q*(-1+q)^(-2)+(-1+q)^(-1))" || b == "-4*(5*(-1+q)^(-2)*q+(-1+q)^(-1))*q" || b == "-4*q*((-1+q)^(-1)+5*(-1+q)^(-2)*q)" || b == "-4*(5*q*(-1+q)^(-2)+(-1+q)^(-1))*q")
})


test_that("pdf2", {
    b <- printFinitizedNegativeBinomialDensity(2,4,2)
    expect_true( b == "10*(-1+q)^(-2)*q^2" || b == "10*q^2*(-1+q)^(-2)")
})

test_that("pdf3", {
    expect_equal(printFinitizedNegativeBinomialDensity(2,4,3),"0")
})


test_that("pdf2L", {
    expect_equal(printFinitizedBinomialDensity(2,4,2, TRUE),"6  p^{2}")
})

test_that("pdf3L", {
    expect_equal(printFinitizedBinomialDensity(2,4,3, TRUE),"0")
})

