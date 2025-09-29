
#' Prints and returns the string representation of the density function.
#'
#' Prints and returns the density function for finitized probability distributions.
#'
#' @param n The finitization order.
#' @param val The value of the variable for which the probability density function is printed. If NULL, this function computes the
#' string representation of the pdf for all possible values, i.e. \code{{0 .. n}}.
#' @param params The parameters of the finitized distribution. \code{params} is a named list containig the parameters needed for a specific distribution:
#' the name of an item is the name of the parameters, and the value of an item is the value of the corresponding parameter.
#' @param type The distribution type. It could have one of the values returned by:
#' \itemize{
#' \item getPoissonType()
#' \item getBinomialType()
#' \item getNegativeBinomialType()
#' \item getLogarithmicType()
#' }
#' @param latex if TRUE, a Latex string of the pdf is printed and returned, otherwise the string has the format of an R expression.
#' @keywords internal
#' @return the string representation of the finitized probability distribution function.
printDensity <- function(n, val, params, type, latex) {
    if(is.null(val))
        lim <- 0:n
    else
        lim <- val
    cat(paste0("X", "\t", "pdf\n"))
    result <- c_printDensity(n, lim, params, type, latex)
    k <- 1
    for (i in lim) {
        cat(paste0(i, '\t', result[k], '\n'))
        k <- k + 1
    }
    return(result)
}




#' Checks the validity of the parameter \code{theta} of the finitized Poisson and/or Logarithmic distribution.
#'
#' Checks if the parameter of the finitized Poisson distribution satisfies \code{length(theta) == 1} (no vectors with more than one element are allowed),
#' and if it is a double with values in \code{[0,1]}. If the value passed through this parameter does not meet these criteria, the function returns FALSE, otherwise it
#' returns TRUE.
#'
#' @param theta The parameter of the finitized Poisson distribution.
#' @keywords internal
#' @return TRUE if \code{length(theta) == 1} and \code{theta} is a double in\code{[0,1]} interval, FALSE otherwise.
checkTheta <- function(theta) {
    result = TRUE
    if(length(theta) != 1)  {
        message(paste0("Invalid argument: ", theta))
        result = FALSE
    }
    if (!is.double(theta)) {
        message("theta should be a double\n")
        result = FALSE
    } else {
        if ( !(theta >= 0  && theta <= 1) ) {
            message("The parameter theta should have values between 0 and 1\n")
            result = FALSE
        }
    }
    return(result)
}


#' Checks the validity of the parameter \code{p} (the rate of success) of the finitized Binomial distribution.
#'
#' Checks if the parameter \code{p} of the finitized Binomial distribution satisfies \code{length(p) == 1} (no vectors with more than one element are allowed),
#' and if it is a double with values in \code{[0,1]}. If the value passed through this parameter does not meet these criteria, the function returns FALSE, otherwise it
#' returns TRUE.
#'
#' @param p The parameter of the finitized Binomial distribution.
#' @keywords internal
#' @return TRUE if \code{length(p) == 1} and \code{p} is a double in \code{[0,1]} interval, FALSE otherwise.
checkBinomialP <- function(p) {
    result = TRUE
    if(length(p) != 1) {
        message(paste0("Invalid argument: ", p))
        result = FALSE
    }
    if (!is.double(p)) {
        message("p should be a double\n")
        result = FALSE
    } else {
        if ( !(p >= 0  && p <= 1) ) {
            message("The parameter of the finitized Binomial distribution should have values between 0 and 1\n")
            result = FALSE
        }
    }
    return(result)
}


#' Checks the validity of the parameter \code{q} (the rate of success) of the finitized Negative Binomial distribution.
#'
#' Checks if the parameter \code{q} of the finitized Negative Binomial distribution satisfies \code{length(q) == 1} (no vectors with more than one element are allowed),
#' and if it is a double with values in \code{[0,1]}. If the value passed through this parameter does not meet these criteria, the function returns FALSE, otherwise it
#' returns TRUE.
#'
#' @param q The parameter of the finitized Negative Binomial distribution.
#' @keywords internal
#' @return TRUE if \code{length(q) == 1} and \code{q} is a double in \code{[0,1]} interval, FALSE otherwise.
checkNegBinomialQ <- function(q) {
    result = TRUE
    if(length(q) != 1 ) {
        message(paste0("Invalid argument: ", q))
        result = FALSE
    }
    if (!is.double(q)) {
        message("q should be a double\n")
        result = FALSE
    } else {
        if ( !(q >= 0  && q <= 1) ) {
            message("The parameter of the finitized Negative Binomial distribution should have values between 0 and 1\n")
            result = FALSE
        }
    }
    return(result)
}

#' Checks if the values of the variable are valid.
#'
#' Checks if the values of the variable are valid, i.e. they belong to \code{{0, 1, 2, ... n}}, where \code{n} is the finitization order.
#'
#' @param n The finitization order of the distribution
#' @param val a vector with the values of the variable.
#' @keywords internal
#' @return TRUE if all values in \code{val} are integers and they are in the set \code{{0, 1, 2, ... n}}, FALSE otherwise.
checkVals <- function(n, val) {
    result = TRUE
    if(is.logical(val)) {
        result = FALSE
        message("Parameter val should have only integer values\n")
    }
    if( sum(sapply(val,trunc) != val) != 0  ) {
        message("Parameter val should have only integer values\n")
        result = FALSE
    }

    if (result) {
        if (!(all(val >= 0 ) && all(val <= n))) {
            message(paste0("val should be an integer or a vector of integers with values between 0 and ", n, "\n"))
            result = FALSE
        }
    }
    return(result)
}

#' Find maximum feasible parameter space (MFPS) bounds
#'
#' This helper locates the two rightmost roots of the polynomial
#' \eqn{pdf(n-1)} that define the maximum feasible parameter space (MFPS)
#' of a finitized distribution.
#'
#' The algorithm works in two stages:
#' \enumerate{
#'   \item An initial dense search in the interval \code{[lower, upper]} using
#'   \code{\link[rootSolve]{uniroot.all}}, keeping only the two largest roots.
#'   Endpoints are treated as roots if the function value is numerically close
#'   to zero (within \code{eps_endpoint}).
#'   \item If the largest root may lie to the right of \code{upper}, the search
#'   expands rightward in geometrically increasing segments, first with a coarse
#'   grid then refined with a finer grid. Expansion stops when no additional
#'   roots are found or when \code{max_upper} is reached.
#' }
#'
#' The result is the interval \eqn{[\mathrm{LL}, \mathrm{UL}]}, where \eqn{UL}
#' is the largest detected root and \eqn{LL} is the second-largest root (or
#' \eqn{0} if only one root is found).
#'
#' @param func A function of one variable (typically \code{theta}) representing
#'   the polynomial \eqn{pdf(n-1)} whose roots determine the MFPS.
#' @param lower Numeric, left endpoint of the initial search interval
#'   (default \code{0}).
#' @param upper Numeric, right endpoint of the initial search interval
#'   (default \code{1}).
#' @param eps_endpoint Numeric tolerance for treating an endpoint as a root
#'   (default \code{1e-10}).
#' @param tol Numeric tolerance passed to \code{\link[rootSolve]{uniroot.all}}
#'   (default \code{1e-8}).
#' @param initial_n Integer grid size for the initial dense search in
#'   \code{[lower, upper]} (default \code{1e7}).
#' @param growth Numeric growth factor for rightward expansion (default \code{2}).
#' @param max_upper Maximum right endpoint for expansion (default \code{1}).
#' @param coarse_pts Number of coarse grid points in rightward probing
#'   (default \code{256}).
#' @param fine_pts Number of fine grid points in rightward probing
#'   (default \code{4096}).
#'
#' @return A numeric vector of length two: \code{c(LL, UL)}, the lower and upper
#'   bounds of the maximum feasible parameter space. Returns \code{c(NA, NA)}
#'   if no roots are found.
#'
#' @keywords internal
#'
#' @examples
#' ## Example 1: Well-conditioned polynomial with exact roots at 0 and 1
#' f1 <- function(theta) (2.2e-16) * theta^12 * (1 - theta)
#' findSolutions(f1, initial_n = 1e5, tol = 1e-10)
#'
#' ## Example 2: Endpoint snapping (near-zero at 0 and 1 within eps)
#' f2 <- function(theta) 1e-14 * theta^8 * (theta - 1) + 1e-20
#' # Treat endpoint values within eps as zeros:
#' findSolutions(f2, eps_endpoint = 1e-12, initial_n = 2e5)
#'
#' ## Example 3: Largest root beyond 1 → rightward expansion finds it
#' ##            (toy function with roots near 0.3 and 1.2)
#' f3 <- function(theta) (theta - 0.3) * (theta - 1.2)
#' # Start in [0,1], then expand to the right up to max_upper = 2
#' findSolutions(f3, lower = 0, upper = 1, max_upper = 2,
#'               initial_n = 2e5, coarse_pts = 128L, fine_pts = 1024L)
#'
#' ## Example 4 (MFPS flavor): A PSD-style series truncated to n−1
#' ## Suppose pdf(n-1) reduces to a low-degree polynomial in theta:
#' f4 <- function(theta) 1 - 4*theta + 10*theta^2 - 20*theta^3 + 35*theta^4
#' # Return the two rightmost admissible theta values as MFPS bounds
#' findSolutions(f4, lower = 0, upper = 1, initial_n = 1e7, tol = 1e-10)
findSolutions <- function(func,
                          lower = 0, upper = 1,
                          eps_endpoint = 1e-10,  # treat endpoint as root if |f| <= eps
                          tol = 1e-8,            # uniroot() tolerance
                          initial_n = 1e7,       # grid points in [lower, upper]
                          growth = 2,            # rightward geometric growth
                          max_upper = 1e0,       # cap for right expansion
                          coarse_pts = 256L,     # coarse grid per right segment
                          fine_pts   = 4096L) {  # fine grid after coarse hit
    stopifnot(lower < upper, growth > 1,
              initial_n >= 1e3, coarse_pts >= 16L, fine_pts > coarse_pts)

    f0 <- suppressWarnings(func(lower))
    f1 <- suppressWarnings(func(upper))
    # Decide if endpoints are (near) roots
    has0 <- is.finite(f0) && abs(f0) <= eps_endpoint
    has1 <- is.finite(f1) && abs(f1) <= eps_endpoint


    # --- helper from above ---
    last_two_roots_in <- function(a, b, m) {
        inset <- max(.Machine$double.eps^(1/4), (b - a) * 1e-12)
        a2 <- a + inset; b2 <- b - inset
        if (!(a2 < b2)) return(numeric(0))

        roots <- tryCatch(
            rootSolve::uniroot.all(func, c(a2, b2), n = as.integer(m), tol = tol),
            error = function(e) numeric(0)
        )

        fa <- suppressWarnings(func(a))
        if (is.finite(fa) && abs(fa) <= eps_endpoint) roots <- c(a, roots)
        fb <- suppressWarnings(func(b))
        if (is.finite(fb) && abs(fb) <= eps_endpoint) roots <- c(roots, b)

        roots <- sort(unique(roots))
        if (length(roots) > 2L) roots <- tail(roots, 2L)
        roots
    }

    # 1) Initial dense scan on [lower, upper]
    roots <- last_two_roots_in(lower, upper, m = as.integer(initial_n))
    # If still fewer than 2 roots, snap endpoints when they are (near) zeros
    if (length(roots) < 2L) {
        fU <- suppressWarnings(func(upper))
        if (is.finite(fU) && abs(fU) <= eps_endpoint) roots <- sort(unique(c(roots, upper)))
    }
    if (length(roots) < 2L) {
        fL <- suppressWarnings(func(lower))
        if (is.finite(fL) && abs(fL) <= eps_endpoint) roots <- sort(unique(c(roots, lower)))
    }
    roots <- sort(unique(roots))

    # Initialize LL, UL (two rightmost roots known so far)
    # if (length(roots) == 0L) {
    #     LL <- NA_real_; UL <- NA_real_
    # } else if (length(roots) == 1L) {
    #     LL <- min(lower, roots[1L]); UL <- roots[1L]
    # } else {
    #     LL <- roots[length(roots) - 1L]
    #     UL <- roots[length(roots)]
    # }


    roots_all <- c(if (has0) lower else NULL,
                   roots,
                   if (has1) upper else NULL)

    if (!length(roots_all)) {
        LL<- NA_real_; UL <- NA_real_
    }
    if(length(roots_all) == 1)
        LL = 0
    else
        LL <- min(roots_all)
    UL <- max(roots_all)

    # 2) Expand to the right only if needed (coarse→fine)
    a <- upper
    b <- min(max_upper, max(upper * growth, upper + 1))
    while (a < max_upper) {
        coarse_roots <- last_two_roots_in(a, b, m = as.integer(coarse_pts))
        if (!length(coarse_roots)) break  # no sign change → stop

        fine_roots <- last_two_roots_in(a, b, m = as.integer(fine_pts))
        if (length(fine_roots)) {
            candidates <- sort(unique(c(LL, UL, fine_roots)))
            if (length(candidates) == 1L) {
                LL <- min(lower, candidates[1L]); UL <- candidates[1L]
            } else {
                LL <- candidates[length(candidates) - 1L]
                UL <- candidates[length(candidates)]
            }
        }

        a <- b
        b <- min(max_upper, b * growth)
        if (b <= a) break
    }

    c(LL, UL)
}


## Safe scalar evaluation (clamps x to finite range and swallows warnings)
.fsafe <- function(f, x) {
    y <- suppressWarnings(f(x))
    if (is.finite(y)) y else NA_real_
}


#' Checks if a parameter has an integer value.
#'
#' Checks if the parameter \code{no} satisfies \code{length(N) == 1} (no vectors with more than one element are allowed),
#' and if it is an integer greater than 0. If \code{no} is not an integer number, it will be converted to an integer value.
#' If the value passed through this parameter does not meet these criteria, the function returns FALSE, otherwise it
#' returns TRUE.
#' @param no the parameter to be checked for validity.
#' @keywords internal
#' @return TRUE if \code{length(no) == 1} and \code{no} is an integer greater than 0, FALSE otherwise.
checkIntegerValue <- function(no) {
    result = TRUE
    if(length(no) != 1) {
        message(paste0("Invalid argument: ", no))
        result = FALSE
    }
    if(trunc(no) != no) {
        #warning(paste0(no, " will be converted to an integer"))
        result = FALSE
    }
    if(no < 0) {
        message(paste0("Invalid argument: ", no))
        result = FALSE
    }
    return(result)
}

normalize_expr <- function(expr) {
    expr <- gsub(" ", "", expr)        # remove spaces
    expr <- gsub("\\^1\\b", "", expr)    # drop power of 1
    expr <- gsub("\\*1\\b", "", expr)    # drop *1
    expr
}
