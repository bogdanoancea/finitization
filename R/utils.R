
#' Title

#' @param n
#' @param val
#' @param params
#' @param type
#' @param latex
#'
#' @return
#' @export
#'
#' @examples
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



#' Title
#'
#' @param n
#'
#' @return
#' @export
#'
#' @examples
checkFinitizationOrder <- function(n) {
    result = TRUE
    if(length(n) > 1) {
        message(paste0("invalid argument: ", n))
        result = FALSE
    }
    if (trunc(n) != n) {
        message("n should be an integer number\n")
        result = FALSE
    } else {
        if ( n < 1) {
            message("n should be an integer greater than zero\n")
            result = FALSE
        }
    }
    return(result)
}

#' Title
#'
#' @param theta
#'
#' @return
#' @export
#'
#' @examples
checkPoissonTheta <- function(theta) {
    result = TRUE
    if(length(theta) > 1)  {
        message(paste0("invalid argument: ", theta))
        result = FALSE
    }
    if (!is.double(theta)) {
        message("theta should be a double\n")
        result = FALSE
    } else {
        if ( !(theta > 0  && theta <= 1) ) {
            message("the parameter of the finitized Poisson distribution should be between 0 and 1\n")
            result = FALSE
        }
    }
    return(result)
}

checkLogarithmicTheta <- function(theta) {
    result = TRUE
    if(length(theta) > 1 ) {
        message(paste0("invalid argument: ", theta))
        result = FALSE
    }

    if (!is.double(theta)) {
        message("theta should be a double\n")
        result = FALSE
    } else {
        if ( !(theta > 0  && theta <= 1) ) {
            message("the parameter of the finitized logarithmic distribution should be between 0 and 1\n")
            result = FALSE
        }
    }
    return(result)
}

#' Title
#'
#' @param p
#'
#' @return
#' @export
#'
#' @examples
checkBinomialP <- function(p) {
    result = TRUE
    if(length(p) > 1) {
        message(paste0("invalid argument: ", p))
        result = FALSE
    }
    if (!is.double(p)) {
        message("p should be a double\n")
        result = FALSE
    } else {
        if ( !(p >= 0  && p <= 1) ) {
            message("the parameter of the finitized binomial distribution should be between 0 and 1\n")
            result = FALSE
        }
    }
    return(result)
}


#' Title
#'
#' @param q
#'
#' @return
#' @export
#'
#' @examples
checkNegBinomialQ <- function(q) {
    result = TRUE
    if(length(q) > 1 ) {
        message(paste0("invalid argument: ", q))
        result = FALSE
    }
    if (!is.double(q)) {
        message("q should be a double\n")
        result = FALSE
    } else {
        if ( !(q >= 0  && q <= 1) ) {
            message("the parameter of the finitized negative binomial distribution should be between 0 and 1\n")
            result = FALSE
        }
    }
    return(result)
}

#' Title
#'
#' @param N
#'
#' @return
#' @export
#'
#' @examples
checkBinomialN <- function(N) {
    result = TRUE
    if(length(N) > 1 ) {
        message(paste0("invalid argument: ", N))
        result = FALSE
    }
    if(trunc(N) != N)
        warning(paste0(N, " will be converted to an integer"))
    if(N < 0) {
        message(paste0("invalid argument: ", N))
        result = FALSE
    }
    return(result)
}

#' Title
#'
#' @param k
#'
#' @return
#' @export
#'
#' @examples
checkNegBinomialK <- function(k) {
    result = TRUE
    if(trunc(k) != k)
        warning(paste0(k, " will be converted to an integer"))
    if(k < 0) {
        message(paste0("invalid argument: ", k))
        result = FALSE
    }
    return(result)
}



checkVals <- function(n, val) {
    result = TRUE
    if(is.logical(val)) {
        result = FALSE
        message("val should have only integer values\n")
    }
    if( sum(sapply(val,trunc) != val) != 0  ) {
        message("val should have only integer values\n")
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



checkNoValues <- function(no) {
    result = TRUE
    if(length(no) > 1) {
        message(paste0("invalid argument: ", no))
        result = FALSE
    }
    if(trunc(no) != no)
        warning(paste0(no, " will be converted to an integer"))
    if(no < 0) {
        message(paste0("invalid argument: ", no))
        result = FALSE
    }
    return(result)
}
