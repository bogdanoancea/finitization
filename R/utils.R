
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
    result <- vector(length = length(lim))
    k <- 1
    for (i in lim) {
        cat(paste0(i, '\t'))
        result[k] <- c_printDensity(n, i, params, type, latex)
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

checkVals <- function(n, val) {
    result = TRUE
    if( sum(sapply(val,trunc) != val) != 0  ) {
        message("val should have only integer values\n")
        result = FALSE
    }

    if (result) {
        if (!(val >= 0 && val <= n)) {
            message(paste0("val should be an integer or a vector of integers with values between 0 and ", n, "\n"))
            result = FALSE
        }
    }
    return(result)
}
