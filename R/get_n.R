#' #' TODO:Choose the minimum finitization order n for a target KS accuracy.
#' #' This feature will be implemented in the next version
#' #' @description
#' #' Picks the smallest finitization order \code{n} such that the Kolmogorov–Smirnov
#' #' distance between the parent PSD and its finitized version is at most
#' #' \eqn{c_\alpha / \sqrt{m}}. Supports \code{family = "poisson"|"binomial"|"nbinom"|"log"}.
#' #'
#' #' @param family Character scalar: one of \code{"poisson"}, \code{"binomial"}, \code{"nbinom"}, \code{"log"}.
#' #' @param params List of distribution parameters.
#' #'   - poisson: \code{list(lambda = ...)} (assumed 0 < lambda <= 1 for MFPS)
#' #'   - binomial: \code{list(size = N, p = ...)}
#' #'   - nbinom: \code{list(size = r, prob = p)} (base R parameterization)
#' #'   - log: \code{list(theta = ...)} for PSD form on \eqn{x=0,1,\dots} with \eqn{0<theta<1}
#' #' @param m Positive integer: number of draws you plan to simulate (sets KS tolerance).
#' #' @param alpha Significance level for the KS constant (default 0.05).
#' #' @param n_max Max order to try (default 100).
#' #' @param uniform_params Optional \emph{list of param lists} (same structure as \code{params});
#' #'   if provided, \code{choose_n()} returns the smallest \code{n} that meets the KS target for
#' #'   \emph{all} points in the grid (uniform control).
#' #' @param verbose Logical; print progress.
#' #'
#' #' @return A list with the chosen \code{n}, observed KS value(s), the target \code{D_target},
#' #'   and metadata. If the target isn’t met before \code{n_max}, returns the best observed.
#' #'
#' #' @examples
#' #' \dontrun{
#' #' # Poisson (λ ≤ 1)
#' #' finitization::choose_n("poisson", list(lambda = 0.8), m = 1e5)
#' #'
#' #' # Binomial
#' #' finitization::choose_n("binomial", list(size = 20, p = 0.3), m = 5e4)
#' #'
#' #' # Negative Binomial
#' #' finitization::choose_n("nbinom", list(size = 3, prob = 0.4), m = 1e5)
#' #'
#' #' # Logarithmic (PSD form)
#' #' finitization::choose_n("log", list(theta = 0.4), m = 1e5)
#' #'
#' #' # Uniform selection over a parameter grid (Poisson)
#' #' grid <- list(list(lambda=0.2), list(lambda=0.5), list(lambda=0.8), list(lambda=1.0))
#' #' finitization::choose_n("poisson", list(lambda = 1), m = 1e5, uniform_params = grid)
#' #' }
#' #' @export
#' choose_n <- function(family, params, m, alpha = 0.05, n_max = 100,
#'                      uniform_params = NULL, verbose = TRUE) {
#'     stopifnot(is.character(family), length(family) == 1L,
#'               is.list(params), m > 0, n_max >= 0)
#'
#'     # --- KS constants (asymptotic one-sample)
#'     ks_c_alpha <- function(alpha = 0.05) {
#'         if (alpha <= 0.011) 1.63 else if (alpha <= 0.051) 1.36 else 1.22
#'     }
#'     D_target <- ks_c_alpha(alpha) / sqrt(m)
#'
#'     # --- Parent pmf, finitized pmf (from your package), MFPS, and CDF grid size
#'     family <- match.arg(tolower(family), c("poisson","binomial","nbinom","log"))
#'
#'     parent_pmf <- switch(
#'         family,
#'         poisson = function(x, p) stats::dpois(x, lambda = p$lambda),
#'         binomial = function(x, p) stats::dbinom(x, size = p$size, prob = p$p),
#'         nbinom = function(x, p) stats::dnbinom(x, size = p$size, prob = p$prob),
#'         log = function(x, p) { # PSD form on x=0,1,2,...
#'             th <- p$theta
#'             if (th <= 0 || th >= 1) return(rep(0, length(x)))
#'             w <- th / (-log1p(-th))
#'             w * th^x / (x + 1)
#'         }
#'     )
#'
#'     finitized_pmf <- switch(
#'         family,
#'         poisson = function(x, p, n) finitization::dpois_fin(x, lambda = p$lambda, n = n),
#'         binomial = function(x, p, n) finitization::dbinom_fin(x, size = p$size, prob = p$p, n = n),
#'         nbinom = function(x, p, n) finitization::dnbinom_fin(x, size = p$size, prob = p$prob, n = n),
#'         log = function(x, p, n) finitization::dlog_psd_fin(x, theta = p$theta, n = n)
#'     )
#'
#'     mfps_ok <- switch(
#'         family,
#'         poisson = function(p, n) p$lambda > 0 && p$lambda <= 1,               # MFPS often 0<λ≤1
#'         binomial = function(p, n) p$size >= 0 && p$p > 0 && p$p < 1,          # wide MFPS
#'         nbinom = function(p, n) p$size > 0 && p$prob > 0 && p$prob < 1,       # wide MFPS
#'         log = function(p, n) p$theta > 0 && p$theta < 1                        # MFPS depends on n; keep permissive
#'     )
#'
#'     grid_upper <- switch(
#'         family,
#'         poisson = function(p, n) max(n + 20L, stats::qpois(0.999999, p$lambda) + 5L),
#'         binomial = function(p, n) max(n, p$size),
#'         nbinom = function(p, n) {
#'             qq <- stats::qnbinom(0.999999, size = p$size, prob = p$prob)
#'             max(n + 20L, qq + 5L)
#'         },
#'         log = function(p, n) {
#'             th <- p$theta
#'             max(n + 40L, ceiling(10 / (1 - th))) # crude but safe upper grid
#'         }
#'     )
#'
#'     # --- KS on 0..U
#'     ks_gap <- function(p, n) {
#'         U <- grid_upper(p, n)
#'         xs <- 0:U
#'         p_par <- parent_pmf(xs, p)
#'         p_fin <- finitized_pmf(xs, p, n)
#'         p_fin[p_fin < 0] <- 0
#'         s_fin <- sum(p_fin)
#'         if (abs(s_fin - 1) > 1e-8) p_fin <- p_fin / max(s_fin, .Machine$double.eps)
#'         F_par <- cumsum(p_par)
#'         F_fin <- cumsum(p_fin)
#'         max(abs(F_par - F_fin))
#'     }
#'
#'     # --- Search logic
#'     if (is.null(uniform_params)) {
#'         best <- list(n = NA_integer_, Dn = Inf)
#'         for (n in 0:n_max) {
#'             if (!mfps_ok(params, n)) {
#'                 if (verbose) message(sprintf("[%s] n=%d skipped (outside MFPS)", family, n))
#'                 next
#'             }
#'             Dn <- ks_gap(params, n)
#'             if (verbose) message(sprintf("[%s] n=%2d | KS = %.6g | target = %.6g", family, n, Dn, D_target))
#'             if (Dn < best$Dn) best <- list(n = n, Dn = Dn)
#'             if (Dn <= D_target) {
#'                 return(list(
#'                     family = family, params = params,
#'                     n = n, KS = Dn, D_target = D_target,
#'                     mode = "pointwise", status = "ok"
#'                 ))
#'             }
#'         }
#'         warning("Target not met within n_max; returning the best observed.")
#'         return(c(
#'             list(family = family, params = params, mode = "pointwise", status = "n_max_reached"),
#'             best, list(D_target = D_target)
#'         ))
#'     } else {
#'         # Uniform control over a parameter grid
#'         stopifnot(is.list(uniform_params), length(uniform_params) >= 1L)
#'         for (n in 0:n_max) {
#'             Dmax <- 0
#'             ok <- TRUE
#'             for (p in uniform_params) {
#'                 if (!mfps_ok(p, n)) { ok <- FALSE; Dmax <- Inf; break }
#'                 Dn <- ks_gap(p, n)
#'                 Dmax <- max(Dmax, Dn)
#'                 if (verbose) message(sprintf("[%s] n=%d | KS@grid ≤ %.6g (curr=%.6g)", family, n, Dmax, Dn))
#'                 if (!is.finite(Dmax) || Dmax > D_target) { ok <- FALSE; break }
#'             }
#'             if (ok) {
#'                 return(list(
#'                     family = family, params_grid = uniform_params,
#'                     n = n, KS_max = Dmax, D_target = D_target,
#'                     mode = "uniform", status = "ok"
#'                 ))
#'             }
#'         }
#'         warning("Uniform target not met within n_max; returning last Dmax.")
#'         return(list(
#'             family = family, params_grid = uniform_params,
#'             n = NA_integer_, KS_max = Dmax, D_target = D_target,
#'             mode = "uniform", status = "n_max_reached"
#'         ))
#'     }
#' }
#'
#'
#' #' Plot CDF diagnostics for a finitized distribution
#' #'
#' #' @description
#' #' Overlays the parent and finitized CDFs for a given \code{family}, \code{params}, and finitization
#' #' order \code{n}, and annotates the Kolmogorov–Smirnov gap \eqn{D_n = \sup_x |F_n(x)-F(x)|}.
#' #'
#' #' @param family Character: "poisson", "binomial", "nbinom", or "log" (PSD form, x = 0,1,2,...).
#' #' @param params List of distribution parameters (same structure as in \code{choose_n()}).
#' #' @param n Integer finitization order.
#' #' @param main Optional plot title.
#' #' @param col_parent,col_fin Colors for parent and finitized CDF lines.
#' #' @param lwd_parent,lwd_fin Line widths.
#' #' @param pch_max Point symbol at the KS-max location.
#' #' @return (Invisibly) a list with \code{Dn}, \code{x_max}, and the vectors used to plot.
#' #' @export
#' plot_finitization_diagnostics <- function(
#'         family, params, n,
#'         main = NULL,
#'         col_parent = "black", col_fin = "blue",
#'         lwd_parent = 2, lwd_fin = 2,
#'         pch_max = 19
#' ) {
#'     stopifnot(is.character(family), is.list(params), length(n) == 1L, n >= 0)
#'     family <- match.arg(tolower(family), c("poisson","binomial","nbinom","log"))
#'
#'     # Parent pmf
#'     parent_pmf <- switch(
#'         family,
#'         poisson = function(x, p) stats::dpois(x, lambda = p$lambda),
#'         binomial = function(x, p) stats::dbinom(x, size = p$size, prob = p$p),
#'         nbinom = function(x, p) stats::dnbinom(x, size = p$size, prob = p$prob),
#'         log = function(x, p) { # PSD form on x = 0,1,2,...
#'             th <- p$theta
#'             if (th <= 0 || th >= 1) return(rep(0, length(x)))
#'             w <- th / (-log1p(-th))
#'             w * th^x / (x + 1)
#'         }
#'     )
#'
#'     # Finitized pmf (your package)
#'     finitized_pmf <- switch(
#'         family,
#'         poisson = function(x, p, n) finitization::dpois_fin(x, lambda = p$lambda, n = n),
#'         binomial = function(x, p, n) finitization::dbinom_fin(x, size = p$size, prob = p$p, n = n),
#'         nbinom  = function(x, p, n) finitization::dnbinom_fin(x, size = p$size, prob = p$prob, n = n),
#'         log     = function(x, p, n) finitization::dlog_psd_fin(x, theta = p$theta, n = n)
#'     )
#'
#'     # Grid upper bound (enough to see parent tail beyond the finitized support)
#'     grid_upper <- switch(
#'         family,
#'         poisson = function(p, n) max(n + 20L, stats::qpois(0.999999, p$lambda) + 5L),
#'         binomial = function(p, n) max(n, p$size),
#'         nbinom  = function(p, n) max(n + 20L, stats::qnbinom(0.999999, size = p$size, prob = p$prob) + 5L),
#'         log     = function(p, n)  max(n + 40L, ceiling(10 / (1 - p$theta)))
#'     )
#'
#'     U <- grid_upper(params, n)
#'     xs <- 0:U
#'     p_par <- parent_pmf(xs, params)
#'     p_fin <- finitized_pmf(xs, params, n)
#'
#'     # clean-up & normalization (for numerical safety)
#'     p_fin[p_fin < 0] <- 0
#'     s_fin <- sum(p_fin)
#'     if (abs(s_fin - 1) > 1e-8) p_fin <- p_fin / max(s_fin, .Machine$double.eps)
#'
#'     F_par <- cumsum(p_par)
#'     F_fin <- cumsum(p_fin)
#'     diff  <- abs(F_par - F_fin)
#'     Dn    <- max(diff)
#'     i_max <- which.max(diff)
#'     x_max <- xs[i_max]
#'
#'     if (is.null(main)) {
#'         main <- sprintf("CDF diagnostics: %s (n = %d)", family, n)
#'     }
#'
#'     # Plot
#'     plot(xs, F_par, type = "s", lwd = lwd_parent, col = col_parent,
#'          xlab = "x", ylab = "CDF", main = main, ylim = c(0, 1))
#'     lines(xs, F_fin, type = "s", lwd = lwd_fin, col = col_fin)
#'     # KS marker
#'     segments(x0 = x_max, y0 = F_par[i_max], x1 = x_max, y1 = F_fin[i_max],
#'              lty = 2)
#'     points(x_max, F_par[i_max], pch = pch_max, col = col_parent)
#'     points(x_max, F_fin[i_max], pch = pch_max, col = col_fin)
#'     legend("bottomright",
#'            legend = c("Parent CDF", "Finitized CDF", sprintf("KS gap D_n = %.3g", Dn)),
#'            lwd = c(lwd_parent, lwd_fin, 1), col = c(col_parent, col_fin, "gray40"),
#'            lty = c(1,1,2), bty = "n")
#'
#'     invisible(list(Dn = Dn, x_max = x_max, x = xs, F_parent = F_par, F_finitized = F_fin))
#' }
