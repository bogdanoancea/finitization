#' @include utils.R

#' @param n The finitization order. It should be an integer > 0.
#'
#' @param theta The parameter of the Poisson distribution.
#'
#' @param val The value of the variable for which the probability density function is computed. If NULL, a data frame containing
#' all possible values, i.e. {0 .. n}, and the corresponding probabilities is returned.
#'
#' @export
dpois <- function(n, theta, val = NULL) {
    if ( !checkFinitizationOrder(n))
        return(invisible(NULL) )
    if ( !checkPoissonTheta(theta) )
        return(invisible(NULL))
    if (!is.null(val)) {
        if( !checkVals(n, val))
            return(invisible(NULL))
        lim <- val
    } else {
        lim <- seq(0,n)
    }
    d <- c_d(n, lim, list("theta" = theta), getPoissonType())
    df <- data.frame(val = lim, prob = d)
    return(df)
}

#' @param n The finitization order. It should be an integer > 0.
#' @param val The value of the variable for which the probability density function is printed. If NULL, this function prints the
#' pdf for all possible values, i.e. {0 .. n}.
#' @param latex If TRUE, a string representation of the pdf formatted in Latex format is printed, otherwise it prints
#' the string representation of the pdf as an R expression.
#' @export
printFinitizedPoissonDensity <-
    function(n, val = NULL, latex = FALSE)  {
        if(!checkFinitizationOrder(n))
            return(NULL)
        if( !is.null(val) && !checkVals(n, val))
            return(invisible(NULL))

        r <- printDensity(n, val, NULL, getPoissonType(), latex)
        return(r)
    }

#' @param  n The finitization order. It should be an integer > 0.
#' @export
getPoissonMFPS <- function(n) {
    fg <- function(theta) {
        "x"
    }
    body(fg)[[2]] <- parse(text = MFPS_pois_pdf(n))[[1]]
    U <- 1
    L <- 0

    while (is.infinite(fg(U)))
        U <- U - .Machine$double.eps
    while (is.infinite(fg(L)))
        L <- L + .Machine$double.eps
    solutions <-
        rootSolve::uniroot.all(fg, c(U, L), n = 10 ^ 7, tol = .Machine$double.eps)
    UL = solutions[length(solutions)]
    if (length(solutions) > 1)
        LL = solutions[length(solutions) - 1]
    else
        LL = 0
    return(c(min(LL, UL), max(LL, UL)))
}
