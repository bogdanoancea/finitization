# tests/testthat/test-pois-consistency.R

test_that("ppois consistency with dpois (finitized Poisson)", {
    n <- 5
    lambda <- 0.7  # keep safely in MFPS for small n

    for (k in 0:n) {
        cdf_val <- ppois(n = n, theta = lambda, val = k)$cdf[1]

        pmf_sum <- sum(vapply(
            0:k,
            function(i) dpois(n = n, theta = lambda, val = i)$prob[1],
            numeric(1)
        ))

        expect_equal(cdf_val, pmf_sum, tolerance = 1e-10)
    }
})

test_that("qpois is a right-continuous inverse of ppois (finitized Poisson)", {
    n <- 5
    lambda <- 0.7

    for (k in 0:n) {
        prob <- ppois(n = n, theta = lambda, val = k)$cdf[1]
        q    <- qpois(n = n, theta = lambda, p = prob)

        expect_true(q >= 0 && q <= n)

        Fq <- ppois(n = n, theta = lambda, val = q)$cdf[1]
        expect_true(Fq >= prob - 1e-10)

        if (q > 0) {
            Fqm1 <- ppois(n = n, theta = lambda, val = q - 1)$cdf[1]
            expect_true(Fqm1 < prob + 1e-10)
        }
    }
})

test_that("rpois empirical frequencies agree with dpois (finitized Poisson)", {
    set.seed(1)

    n <- 5
    lambda <- 0.7
    no <- 200000

    x <- rpois(n = n, theta = lambda, no = no)

    expect_true(all(x >= 0))
    expect_true(all(x <= n))

    emp <- tabulate(x + 1, nbins = n + 1) / no
    th  <- vapply(0:n, function(k) dpois(n = n, theta = lambda, val = k)$prob[1], numeric(1))

    z <- 4  # conservative
    tol <- z * sqrt(th * (1 - th) / no) + 5e-4

    expect_true(all(abs(emp - th) <= tol))
})
