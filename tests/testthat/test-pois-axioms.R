# tests/testthat/test-pois-axioms.R

test_that("Finitized Poisson satisfies probability axioms (pmf sums to 1, bounds, CDF hits 1)", {

    # A few representative (n, lambda) pairs inside the MFPS.
    # For Poisson NTSF, MFPS is typically lambda in (0,1]; keep well inside.
    cases <- list(
        list(n = 1, lambda = 0.20),
        list(n = 2, lambda = 0.70),
        list(n = 4, lambda = 0.95),
        list(n = 8, lambda = 0.40)
    )

    tol_sum <- 1e-12
    tol_cdf <- 1e-12

    for (cs in cases) {
        n <- cs$n
        lambda <- cs$lambda
        k <- 0:n

        # PMF values for all support points
        pmf_df <- dpois(n = n, theta = lambda, val = k)
        pmf <- pmf_df$prob

        # 1) Sum to 1
        expect_equal(sum(pmf), 1.0, tolerance = tol_sum)

        # 2) Each pmf in [0,1]
        expect_true(all(is.finite(pmf)))
        expect_true(all(pmf >= -tol_sum))  # allow tiny negative due to floating error
        expect_true(all(pmf <= 1 + tol_sum))

        # 3) CDF hits 1 at upper bound
        cdf_df <- ppois(n = n, theta = lambda, val = n)
        expect_equal(cdf_df$cdf[1], 1.0, tolerance = tol_cdf)

        # Extra sanity: CDF never exceeds 1 over support
        cdf_all <- ppois(n = n, theta = lambda, val = k)$cdf
        expect_true(all(cdf_all <= 1 + tol_sum))
        expect_true(all(cdf_all >= -tol_sum))
    }
})
