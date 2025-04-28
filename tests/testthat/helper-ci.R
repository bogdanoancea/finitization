# tests/testthat/helper-ci.R

#' Check if a sample statistic lies within a confidence interval
#'
#' @param observed The observed sample statistic (e.g., sample mean, sample variance).
#' @param expected The theoretical expected value.
#' @param se Standard error of the sample statistic.
#' @param center Whether to center the CI around "expected" or "sample".
#' @param level Confidence level (default 0.99).
#' @param tolerance Extra floating point wiggle room (default 1e-3).
#' @param info Custom message (optional).
#'
#' @return A testthat::expect_true assertion.
expect_within_ci <- function(
        observed,
        expected,
        se,
        center = c("expected", "sample"),
        level = 0.99,
        tolerance = 1e-3,
        info = NULL
) {
    center <- match.arg(center)
    alpha <- 1 - level
    z_value <- qnorm(1 - alpha/2)

    if (center == "expected") {
        ci_low <- expected - z_value * se
        ci_high <- expected + z_value * se
        test_target <- observed
    } else {
        ci_low <- observed - z_value * se
        ci_high <- observed + z_value * se
        test_target <- expected
    }

    if (is.null(info)) {
        info <- paste0(
            "Observed = ", round(observed, 5),
            "; Expected = ", round(expected, 5),
            "; CI [", round(ci_low, 5), ", ", round(ci_high, 5), "] centered on ", center
        )
    }

    testthat::expect_true(
        test_target >= (ci_low - tolerance) && test_target <= (ci_high + tolerance),
        info = info
    )
}
