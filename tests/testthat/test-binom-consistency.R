# tests/testthat/test-binom-consistency.R

test_that("pbinom consistency with dbinom (finitized binomial)", {
    n <- 5
    N <- 10
    p <- 0.1

    for (k in 0:n) {
        # Your package returns data frames; extract scalar values
        cdf_val <- pbinom(n = n, p = p, N = N, val = k)$cdf[1]

        pmf_sum <- sum(vapply(
            0:k,
            function(i) dbinom(n = n, p = p, N = N, val = i)$prob[1],
            numeric(1)
        ))

        expect_equal(cdf_val, pmf_sum, tolerance = 1e-10)
    }
})

test_that("qbinom is a right-continuous inverse of pbinom (finitized binomial)", {
    n <- 5
    N <- 10
    p <- 0.1

    for (k in 0:n) {
        prob <- pbinom(n = n, p = p, N = N, val = k)$cdf[1]
        q    <- qbinom(n = n, p = p, N = N, prob = prob)

        # Right-continuous inverse properties:
        # 1) returned quantile should be <= upper support bound
        expect_true(q >= 0 && q <= n)

        # 2) F(q) >= prob
        Fq <- pbinom(n = n, p = p, N = N, val = q)$cdf[1]
        expect_true(Fq >= prob - 1e-10)

        # 3) if q > 0 then F(q-1) < prob (minimality)
        if (q > 0) {
            Fqm1 <- pbinom(n = n, p = p, N = N, val = q - 1)$cdf[1]
            expect_true(Fqm1 < prob + 1e-10)
        }
    }
})

test_that("rbinom empirical frequencies agree with dbinom (finitized binomial)", {
    set.seed(1)

    n <- 5
    N <- 10
    p <- 0.1
    no <- 200000

    x <- rbinom(n = n, p = p, N = N, no = no)

    # Support checks
    expect_true(all(x >= 0))
    expect_true(all(x <= n))

    # Empirical frequencies
    emp <- tabulate(x + 1, nbins = n + 1) / no

    # Theoretical pmf
    th <- vapply(0:n, function(k) dbinom(n = n, p = p, N = N, val = k)$prob[1], numeric(1))

    # Compare with a simple normal-approx CI around each prob:
    # |p_hat - p| <= z * sqrt(p(1-p)/no) + small eps
    z <- 4  # conservative
    tol <- z * sqrt(th * (1 - th) / no) + 5e-4

    expect_true(all(abs(emp - th) <= tol))
})
