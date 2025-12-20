# tests/testthat/test-log-consistency.R

test_that("plog consistency with dlog (finitized logarithmic)", {
    n <- 5
    theta <- 0.10  # keep safely inside MFPS for small n

    for (x in 0:n) {
        cdf_val <- plog(n = n, theta = theta, val = x)$cdf[1]

        pmf_sum <- sum(vapply(
            0:x,
            function(i) dlog(n = n, theta = theta, val = i)$prob[1],
            numeric(1)
        ))

        expect_equal(cdf_val, pmf_sum, tolerance = 1e-10)
    }
})

test_that("qlog is a right-continuous inverse of plog (finitized logarithmic)", {
    n <- 5
    theta <- 0.10

    for (x in 0:n) {
        prob <- plog(n = n, theta = theta, val = x)$cdf[1]
        Q    <- qlog(n = n, theta = theta, p = prob)

        expect_true(Q >= 0 && Q <= n)

        FQ <- plog(n = n, theta = theta, val = Q)$cdf[1]
        expect_true(FQ >= prob - 1e-10)

        if (Q > 0) {
            FQm1 <- plog(n = n, theta = theta, val = Q - 1)$cdf[1]
            expect_true(FQm1 < prob + 1e-10)
        }
    }
})

test_that("rlog empirical frequencies agree with dlog (finitized logarithmic)", {
    set.seed(1)

    n <- 5
    theta <- 0.10
    no <- 200000

    x <- rlog(n = n, theta = theta, no = no)

    expect_true(all(x >= 0))
    expect_true(all(x <= n))

    emp <- tabulate(x + 1, nbins = n + 1) / no
    th  <- vapply(0:n, function(xx) dlog(n = n, theta = theta, val = xx)$prob[1], numeric(1))

    z <- 4
    tol <- z * sqrt(th * (1 - th) / no) + 5e-4

    expect_true(all(abs(emp - th) <= tol))
})
