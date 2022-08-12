printDensity <- function(n, val, params, type, latex) {
    if(!is.null(val)) {
        cat(paste0("X", "\t", "pdf\n"))
        cat(paste0(val,":", '\t'))
        x <- c_printDensity(n, val, params, type, latex)
    } else {
        cat(paste0("X", "\t", "pdf\n"))
        for (i in 0:n) {
            cat(paste0(i,":", '\t'))
            x <- c_printDensity(n, i, params, type, latex)
        }
    }
}
