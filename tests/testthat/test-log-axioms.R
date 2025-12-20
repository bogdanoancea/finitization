# tests/testthat/test-log-axioms.R

test_that("Finitized Logarithmic satisfies probability axioms (pmf sums to 1, bounds, CDF hits 1)", {

    # Choose (n, theta) safely inside MFPS. For Logarithmic, MFPS can be tight,
    # so we stay conservative rather than sitting on the boundary.
    cases <- list(
        list(n = 2, theta = 0.10),
        list(n = 4, theta = 0.20),
        list(n = 8, theta = 0.10)
    )

    tol_sum <- 1e-12
    tol_cdf <- 1e-12

    for (cs in cases) {
        n     <- cs$n
        theta <- cs$theta
        k     <- 0:n

        # PMF over full support
        pmf_df <- dlog(n = n, theta = theta, val = k)
        pmf <- pmf_df$prob

        # 1) Sum to unity
        expect_equal(sum(pmf), 1.0, tolerance = tol_sum)

        # 2) Valid probability values
        expect_true(all(is.finite(pmf)))
        expect_true(all(pmf >= -tol_sum))     # allow tiny numerical negatives
        expect_true(all(pmf <= 1 + tol_sum))

        # 3) CDF reaches 1 at upper bound
        cdf_df <- plog(n = n, theta = theta, val = n)
        expect_equal(cdf_df$cdf[1], 1.0, tolerance = tol_cdf)

        # Extra: monotone CDF within [0,1]
        cdf_all <- plog(n = n, theta = theta, val = k)$cdf
        expect_true(all(diff(cdf_all) >= -tol_sum))
        expect_true(all(cdf_all >= -tol_sum))
        expect_true(all(cdf_all <= 1 + tol_sum))
    }
})
