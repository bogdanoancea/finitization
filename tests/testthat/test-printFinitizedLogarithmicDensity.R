test_that("pdf0", {
    b <- printFinitizedLogarithmicDensity(2,0)
    expect_true(b == "1-1/2*theta^2*(2*theta^(-1)*log(1-theta)^(-1)*(-1+theta)^(-1)+log(1-theta)^(-1)*(-1+theta)^(-2)-2*theta^(-2))+(theta^(-1)-log(1-theta)^(-1)*(-1+theta)^(-1))*theta")
})

