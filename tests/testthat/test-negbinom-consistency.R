# tests/testthat/test-negbinom-consistency.R

test_that("pnegbinom consistency with dnegbinom (finitized negative binomial)", {
    n <- 5
    k <- 2
    q <- 0.10  # keep safely inside MFPS for small n,k

    for (x in 0:n) {
        cdf_val <- pnegbinom(n = n, q = q, k = k, val = x)$cdf[1]

        pmf_sum <- sum(vapply(
            0:x,
            function(i) dnegbinom(n = n, q = q, k = k, val = i)$prob[1],
            numeric(1)
        ))

        expect_equal(cdf_val, pmf_sum, tolerance = 1e-10)
    }
})

test_that("qnegbinom is a right-continuous inverse of pnegbinom (finitized negative binomial)", {
    n <- 5
    k <- 2
    q <- 0.10

    for (x in 0:n) {
        prob <- pnegbinom(n = n, q = q, k = k, val = x)$cdf[1]
        Q    <- qnegbinom(n = n, q = q, k = k, p = prob)

        expect_true(Q >= 0 && Q <= n)

        FQ <- pnegbinom(n = n, q = q, k = k, val = Q)$cdf[1]
        expect_true(FQ >= prob - 1e-10)

        if (Q > 0) {
            FQm1 <- pnegbinom(n = n, q = q, k = k, val = Q - 1)$cdf[1]
            expect_true(FQm1 < prob + 1e-10)
        }
    }
})

test_that("rnegbinom empirical frequencies agree with dnegbinom (finitized negative binomial)", {
    set.seed(1)

    n <- 5
    k <- 2
    q <- 0.10
    no <- 200000

    x <- rnegbinom(n = n, q = q, k = k, no = no)

    expect_true(all(x >= 0))
    expect_true(all(x <= n))

    emp <- tabulate(x + 1, nbins = n + 1) / no
    th  <- vapply(0:n, function(xx) dnegbinom(n = n, q = q, k = k, val = xx)$prob[1], numeric(1))

    z <- 4
    tol <- z * sqrt(th * (1 - th) / no) + 5e-4

    expect_true(all(abs(emp - th) <= tol))
})
