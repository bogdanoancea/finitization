# tests/testthat/test-binom-axioms.R

test_that("Finitized Binomial satisfies probability axioms (pmf sums to 1, bounds, CDF hits 1)", {

    # Representative parameter combinations inside MFPS
    cases <- list(
        list(n = 1, N = 5,  p = 0.15),
        list(n = 2, N = 10, p = 0.10),
        list(n = 4, N = 12, p = 0.10),
        list(n = 8, N = 20, p = 0.05)
    )

    tol_sum <- 1e-12
    tol_cdf <- 1e-12

    for (cs in cases) {
        n <- cs$n
        N <- cs$N
        p <- cs$p
        k <- 0:n

        # PMF over full support
        pmf_df <- dbinom(n = n, p = p, N = N, val = k)
        pmf <- pmf_df$prob

        # 1) Sum to unity
        expect_equal(sum(pmf), 1.0, tolerance = tol_sum)

        # 2) Valid probability values
        expect_true(all(is.finite(pmf)))
        expect_true(all(pmf >= -tol_sum))     # allow tiny numerical negatives
        expect_true(all(pmf <= 1 + tol_sum))

        # 3) CDF reaches 1 at upper bound
        cdf_df <- pbinom(n = n, p = p, N = N, val = n)
        expect_equal(cdf_df$cdf[1], 1.0, tolerance = tol_cdf)

        # Extra consistency checks
        cdf_all <- pbinom(n = n, p = p, N = N, val = k)$cdf
        expect_true(all(cdf_all <= 1 + tol_sum))
        expect_true(all(cdf_all >= -tol_sum))
    }
})
