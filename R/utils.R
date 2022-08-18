
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
#'
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
#'
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
#'
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
#'
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
#'
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

#' Solves the equation \eqn{pdf(n-1) = 0} and returns the maximum feasible parameter space.
#'
#' It is used to actually solve the equation \eqn{pdf(n-1) = 0} needed
#' the find the maximum feasible parameter space for a finitized distribution. After finding the solutions, it builds the interval
#' which represents the maximum feasible parameter space.
#' @param func a function. The actual parameter will be \code{pdf(n-1)}.
#'
#' @return a vector with two values: the lower and the upper limits of the maximum feasible parameter space.
findSolutions <- function(func) {
    U <- 1
    L <- 0

    while (is.infinite(func(U)))
        U <- U - .Machine$double.eps
    while (is.infinite(func(L)))
        L <- L + .Machine$double.eps
    solutions <-
        rootSolve::uniroot.all(func, c(U, L), n = 10 ^ 7, tol = .Machine$double.eps)
    UL = solutions[length(solutions)]
    if (length(solutions) > 1)
        LL = solutions[length(solutions) - 1]
    else
        LL = 0
    return(c(min(LL, UL), max(LL, UL)))
}



#' Checks if a parameter has an integer value.
#'
#' Checks if the parameter \code{no} satisfies \code{length(N) == 1} (no vectors with more than one element are allowed),
#' and if it is an integer greater than 0. If \code{no} is not an integer number, it will be converted to an integer value.
#' If the value passed through this parameter does not meet these criteria, the function returns FALSE, otherwise it
#' returns TRUE.
#' @param no the parameter to be checked for validity.
#'
#' @return TRUE if \code{length(no) == 1} and \code{no} is an integer greater than 0, FALSE otherwise.
checkIntegerValue <- function(no) {
    result = TRUE
    if(length(no) != 1) {
        message(paste0("Invalid argument: ", no))
        result = FALSE
    }
    if(trunc(no) != no)
        warning(paste0(no, " will be converted to an integer"))
    if(no < 0) {
        message(paste0("Invalid argument: ", no))
        result = FALSE
    }
    return(result)
}
