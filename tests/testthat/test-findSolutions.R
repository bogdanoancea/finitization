# tests/testthat/test-findSolutions.R

# helper to reach non-exported function during tests
findSolutions <- getFromNamespace("findSolutions", "finitization")

test_that("findSolutions: exact roots at 0 and 1 are found", {
    # f1 has exact roots at 0 and 1
    f1 <- function(theta) (2.2e-16) * theta^12 * (1 - theta)

    b <- findSolutions(
        f1,
        lower = 0, upper = 1,
        initial_n = 1e5,     # small but dense enough
        tol = 1e-10
    )

    expect_length(b, 2L)
    # We expect the last two rightmost roots to be ~ (0, 1)
    expect_true(is.finite(b[1]) && is.finite(b[2]))
    expect_lt(abs(b[1] - 0), 1e-6)
    expect_lt(abs(b[2] - 1), 1e-6)
    expect_lte(b[1], b[2])
})

test_that("findSolutions: endpoint snapping near 0 and 1 works", {
    # f2 is near-zero at endpoints; treat within eps as roots
    f2 <- function(theta) 1e-14 * theta^8 * (theta - 1) + 1e-20

    b <- findSolutions(
        f2,
        lower = 0, upper = 1,
        eps_endpoint = 1e-12, # consider |f| <= 1e-12 as zero at endpoints
        initial_n = 2e6
    )

    expect_length(b, 2L)
    expect_true(is.finite(b[1]) && is.finite(b[2]))
    expect_lt(abs(b[1] - 0), 1e-5)
    expect_lt(abs(b[2] - 1), 1e-5)
    expect_lte(b[1], b[2])
})

test_that("findSolutions: rightward expansion discovers a root beyond 1", {
    # f3 has roots at 0.3 and 1.2 (largest root > 1)
    f3 <- function(theta) (theta - 0.3) * (theta - 1.2)

    b <- findSolutions(
        f3,
        lower = 0, upper = 1,   # start with [0,1] — only sees ~0.3 initially
        max_upper = 2,          # allow expansion up to 2
        initial_n = 2e5,
        coarse_pts = 128L,
        fine_pts   = 1024L,
        tol = 1e-10
    )

    expect_length(b, 2L)
    expect_true(is.finite(b[1]) && is.finite(b[2]))
    # Two rightmost roots should be close to 0.3 and 1.2
    expect_lt(abs(b[1] - 0.3), 1e-4)
    expect_lt(abs(b[2] - 1.2), 1e-4)
    expect_lte(b[1], b[2])
})
