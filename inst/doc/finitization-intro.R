## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4,
  cache = FALSE
)
try(knitr::knit_cache$clear(), silent = TRUE)

## ----echo=FALSE, message=FALSE------------------------------------------------
library(knitr)

comparison <- data.frame(
  Feature = c("Definition", "Support", "Moment preservation",
              "Statistical behavior", "Simulation efficiency", "Interpretation"),
  Truncation = c("Condition on X  <=  n",
                 "{0,1,…,n}",
                 "No – all moments are altered",
                 "Distorted relative to parent distribution",
                 "No special advantage",
                 "Reweighted version of parent distribution"),
  Finitization = c("Construct new distribution on {0,…,n}",
                   "{0,1,…,n}",
                   "Yes – first n moments preserved",
                   "Consistent with parent distribution",
                   "Enables fast aliasing method",
                   "Faithful bounded analog of parent distribution")
)

kable(comparison, format = "markdown", align = "l")

## ----echo=FALSE, results="asis"-----------------------------------------------
library(knitr)

tab1 <- data.frame(
  Distribution = c(
    "Poisson ($\\theta$)",
    "Negative binomial (k=2, $\\theta$)",
    "Binomial (N=4, p), $\\theta$ = p/(1-p)",
    "Logarithmic ($\\theta$)"
  ),
  `Pdf` = c(
    "$f(x)=\\frac{\\theta^x e^{-\\theta}}{x!}$",
    "$f(x)=(x+1)\\theta^x(1-\\theta)^2$",
    "$f(x)=\\binom{4}{x}\\frac{\\theta^x}{(1+\\theta)^4}$",
    "$f(x)=\\frac{\\theta^x}{x+1}\\cdot\\frac{\\theta}{-\\ln(1-\\theta)}$"
  ),
  
  `Series for $\\tau(\\theta)$)` = c(
    "$e^{-\\theta}=1-\\theta+\\theta^2/2-\\theta^3/6+\\cdots$",
    "$1-2\\theta+\\theta^2$",
    "$1-4\\theta+10\\theta^2-20\\theta^3+35\\theta^4+\\cdots$",
    "$1-\\theta/2-\\theta^2/12-\\theta^3/24-19\\theta^4/720+\\cdots$"
  ),
  `Finitized pdf (order 2)` = c(
    "$f_2(0)=1-\\theta+\\theta^2/2$, $f_2(1)=\\theta(1-\\theta)$, $f_2(2)=\\theta^2/2$",
    "$f_2(0)=1-2\\theta+\\theta^2$, $f_2(1)=2\\theta(1-2\\theta)$, $f_2(2)=3\\theta^2$",
    "$f_2(0)=1-4\\theta+10\\theta^2$, $f_2(1)=4\\theta(1-4\\theta)$, $f_2(2)=6\\theta^2$",
    "$f_2(0)=1-\\theta/2-\\theta^2/12$, $f_2(1)=\\tfrac{\\theta}{2}(1-\\theta/2)$, $f_2(2)=\\theta^2/3$"
  ),
  `Parameter restriction` = c(
    "$0<\\theta=\\lambda\\le 1$",
    "$0<2\\theta=\\lambda\\le 1$",
    "$0<4\\theta=\\lambda\\le 1$",
    "$0<\\theta/2=\\lambda\\le 1/2$"
  ),
  stringsAsFactors = FALSE,
  check.names = FALSE   # <- keeps your LaTeX column names
)

kable(tab1, escape = FALSE,
      caption = "Table 1. Common PSDs and their finitized pdfs of order 2 (after Golnabi, Levy & Cochran, 2009).")

## ----echo=FALSE, results="asis"-----------------------------------------------
library(knitr)

tab2 <- data.frame(
  Distribution = c(
    "Poisson($\\lambda$)",
    "Negative binomial (k=2, q)",
    "Binomial (N=4, p)",
    "Logarithmic($\\theta$)"
  ),
  `mgf  ` = c(
    "$e^{\\lambda(e^t - 1)}$",
    "$\\left( \\tfrac{p}{1 - q e^t} \\right)^2$",
    "$\\left( \\tfrac{q + p e^t}{q} \\right)^4$",
    "$\\tfrac{\\ln(1 - \\theta e^t)}{\\ln(1 - \\theta)}$"
  ),
  `NTSD base function $\\delta$(x)` = c(
    "$e^x$",
    "$(1 - x/p)^{-2}$",
    "$(1 + x)^4$",
    "$\\tfrac{\\ln(1 - \\theta - x)}{\\ln(1 - \\theta)}$"
  ),
  `NTSF(2) base function $\\delta_2$(x)` = c(
    "$1 + x + \\tfrac{x^2}{2}$",
    "$1 + \\tfrac{2x}{p} + \\tfrac{3x^2}{p^2}$",
    "$1 + 4x + 6x^2$",
    "$1 + x\\left(\\tfrac{1}{\\theta} - \\tfrac{1}{\\ln(1-\\theta)}\\right) + x^2\\left( \\tfrac{1}{(1-\\theta)^2} - \\tfrac{1}{2\\theta} + \\tfrac{1}{2(\\ln(1-\\theta))^2} \\right)$"
  ),
  `Finitized pdf $f_2(x)$` = c(
    "$f_2(0)=1-\\lambda+\\tfrac{\\lambda^2}{2}$<br>$f_2(1)=\\lambda(1-\\lambda)$<br>$f_2(2)=\\tfrac{\\lambda^2}{2}$",
    "$f_2(0)=\\tfrac{1-4q+6q^2}{(1-q)^2}$<br>$f_2(1)=\\tfrac{2q(4q-1)}{(1-q)^2}$<br>$f_2(2)=\\tfrac{3q^2}{(1-q)^2}$",
    "$f_2(0)=1-4p+6p^2$<br>$f_2(1)=4p(1-3p)$<br>$f_2(2)=6p^2$",
    "$f_2(0)=\\tfrac{4-5\\theta+6\\theta^2}{2\\theta \\ln(1-\\theta)}$<br>$f_2(1)=\\tfrac{4\\theta-3\\theta^2}{2\\theta \\ln(1-\\theta)}$<br>$f_2(2)=\\tfrac{2-3\\theta+2\\theta^2}{2\\theta \\ln(1-\\theta)}$"
  ),
  `Parameter restriction` = c(
    "$0 < \\lambda \\leq 1$",
    "$0 < q \\leq 1/4$",
    "$0 < p \\leq 1/3$",
    "$0 < \\theta \\leq 0.440423$"
  ),
    stringsAsFactors = FALSE,
  check.names = FALSE   # <- keeps your LaTeX column names
)

kable(tab2, format = "markdown", align = "l", caption = "Table 2. Common PSDs and their finitized pdfs of order 2 using the NTFS method")

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("finitization")

## ----message=FALSE------------------------------------------------------------
library(finitization)

## ----eval=FALSE---------------------------------------------------------------
#     Sys.setenv(PATH = paste(
#       "C:/rtools44/ucrt64/bin",
#       Sys.getenv("PATH"),
#       sep = ";"
#     ))

## ----eval=FALSE---------------------------------------------------------------
#  # install.packages("devtools")
#  devtools::install_github("bogdanoancea/finitization")

## -----------------------------------------------------------------------------
library(finitization)

# Parameters used in the examples
n <- 5      # finitization order -> support {0, 1, ..., 5}
q <- 0.10   # success probability per trial (must lie within MFPS)
k <- 2      # number of failures until stopping
x <- 0:n

## -----------------------------------------------------------------------------
# Full PMF table over 0:n
pmf_tbl <- dnegbinom(n, q, k)       # val = NULL -> full table
head(pmf_tbl)

# PMF at selected points
d_points <- dnegbinom(n, q, k, val = c(0, 3, 5))
d_points

# Log-PMF for numerical work
d_log <- dnegbinom(n, q, k, val = 0:3, log = TRUE)
d_log

## -----------------------------------------------------------------------------
# Full CDF table over 0:n
cdf_tbl <- pnegbinom(n, q, k)
head(cdf_tbl)

# Lower-tail CDF at a few points
p_lo <- pnegbinom(n, q, k, val = c(2, 3, 5))
p_lo

# Upper-tail probabilities (P[X > x])
p_hi <- pnegbinom(n, q, k, val = c(2, 3, 5), lower.tail = FALSE)
p_hi

## -----------------------------------------------------------------------------
# Median and 90th percentile
q_med_90 <- qnegbinom(n, q, k, p = c(0.5, 0.9))
q_med_90

# Upper-tail quantile example
q_upper10 <- qnegbinom(n, q, k, p = 0.1, lower.tail = FALSE)
q_upper10

## -----------------------------------------------------------------------------
set.seed(42)
no  <- 5e4
r   <- rnegbinom(n, q, k, no)

# Compare empirical frequencies with the theoretical PMF
freq <- table(factor(r, levels = x)) / no
cbind(x = x, pmf = pmf_tbl$prob, emp = as.numeric(freq))[0:5, ]

## -----------------------------------------------------------------------------
# Quick visual check
plot(x, pmf_tbl$prob, type = "h", lwd = 2, xlab = "x", ylab = "Probability",
     main = "Finitized NB: PMF vs empirical frequencies")
points(x, as.numeric(freq), pch = 16)
legend("topright", bty = "n", legend = c("PMF", "Empirical"), pch = c(NA, 16), lty = c(1, NA))

## -----------------------------------------------------------------------------
mfps <- getNegativeBinomialMFPS(n, k)
length(mfps); head(mfps)

## -----------------------------------------------------------------------------
printFinitizedNegativeBinomialDensity(n, k)
printFinitizedNegativeBinomialDensity(n, k, val = c(0, 1, 2))
printFinitizedNegativeBinomialDensity(n, k, latex = TRUE)

## -----------------------------------------------------------------------------
# PMF/CDF from the finitized NB
pmf <- dnegbinom(n, q, k)                 
cdf <- pnegbinom(n, q, k)

# 1) Normalization of PMF
norm_err <- abs(sum(pmf$density) - 1)
norm_err

# 2) CDF consistency
cdf_from_pmf <- cumsum(pmf$density)
cdf_diff_max <- max(abs(cdf_from_pmf - cdf$cdf))
cdf_diff_max

# 3) Monotonicity of the CDF
is_monotone <- all(diff(cdf$cdf) >= -1e-12)
is_monotone

# 4) Quantile inversion check
p_vec <- c(0.01, 0.10, 0.25, 0.5, 0.9, 0.99)
q_inv <- qnegbinom(n, q, k, p = p_vec)
check_q_ok <- mapply(function(p, qx) {
  lhs <- cdf$cdf[qx + 1L]                  
  rhs <- if (qx > 0) cdf$cdf[qx] else 0    
  (lhs + 1e-12 >= p) && (rhs + 1e-12 < p)
}, p_vec, q_inv)
cbind(p = p_vec, q = q_inv, ok = check_q_ok)

# 5) Simulation vs. PMF moments
set.seed(123)
no <- 1e5
r  <- rnegbinom(n, q, k, no)

mu_th  <- sum(x * pmf$prob)
var_th <- sum((x - mu_th)^2 * pmf$prob)

mu_emp  <- mean(r)
var_emp <- var(r)

cbind(theoretical = c(mean = mu_th, var = var_th),
      empirical    = c(mean = mu_emp, var = var_emp))


## -----------------------------------------------------------------------------
library(finitization)

# Parameters
n <- 5              # finitization order -> claims: first n moments preserved
q <- 0.10           # success probability per trial (package convention)
k <- 2              # number of failures until stopping (package convention)
x <- 0:n            # finite support of the finitized NB

# Parent NB in 'stats' uses: size = k (successes), prob = 1 - q (success probability)
size_parent <- k
prob_parent <- 1 - q

## -----------------------------------------------------------------------------
pmf_raw <- dnegbinom(n, q, k) 
pmf_vec <- pmf_raw$prob
stopifnot(length(pmf_vec) == length(x))
stopifnot(abs(sum(pmf_vec) - 1) < 1e-10)

## -----------------------------------------------------------------------------
# Find M so that tail <= 1e-14
M <- 50L
repeat {
  tail <- 1 - stats::pnbinom(M, size = size_parent, prob = prob_parent)
  if (tail < 1e-14) break
  M <- M * 2L
  if (M > 1e6) stop("Grid grew too large while bounding the tail.")
}
grid <- 0:M
p_parent <- stats::dnbinom(grid, size = size_parent, prob = prob_parent)
stopifnot(abs(sum(p_parent) - 1) < 1e-10)

## -----------------------------------------------------------------------------
# Helper: raw moments from a discrete pmf
raw_moment <- function(x, p, r) sum((x^r) * p)

# Finitized moments on 0:n
mom_fin <- sapply(1:n, function(r) raw_moment(x, pmf_vec, r))

# Parent moments on 0:M (practically exact with very small tail)
mom_par <- sapply(1:n, function(r) raw_moment(grid, p_parent, r))

mom_cmp <- data.frame(
  r = 1:n,
  finitized = mom_fin,
  parent    = mom_par,
  diff      = mom_fin - mom_par
)
mom_cmp

## -----------------------------------------------------------------------------
# Mean and variance separately (these are moments 1 and 2)
mean_fin <- mom_fin[1]
var_fin  <- raw_moment(x, pmf_vec, 2) - mean_fin^2

# Parent mean/var using both summation and closed forms for cross-check
mean_par_sum <- mom_par[1]
var_par_sum  <- (raw_moment(grid, p_parent, 2) - mean_par_sum^2)

# Closed-form moments for stats::NB(size=k, prob=1-q):
# E[X] = k*q/(1-q), Var[X] = k*q/(1-q)^2
mean_par_cf <- k * q / (1 - q)
var_par_cf  <- k * q / (1 - q)^2

data.frame(
  quantity  = c("mean", "variance"),
  finitized = c(mean_fin, var_fin),
  parent_sum= c(mean_par_sum, var_par_sum),
  parent_cf = c(mean_par_cf, var_par_cf),
  diff_fin_parent_cf = c(mean_fin - mean_par_cf, var_fin - var_par_cf)
)

## -----------------------------------------------------------------------------
library(finitization)

# Parameters for the examples
n <- 5      # finitization order -> support {0, 1, ..., 5}
N <- 10     # number of Bernoulli trials in the parent distribution
p <- 0.14    # success probability
x <- 0:n

## -----------------------------------------------------------------------------
# Full PMF over 0:n
pmf_tbl <- finitization::dbinom(n, p, N)
head(pmf_tbl)

# PMF at selected points
d_points <- finitization::dbinom(n, p, N, val = c(0, 2, 5))
d_points

# Log-PMF for selected values
d_log <- finitization::dbinom(n, p, N, val = 0:3, log = TRUE)
d_log

## -----------------------------------------------------------------------------
# Full CDF table over 0:n
cdf_tbl <- finitization::pbinom(n, p, N)
head(cdf_tbl)

# Lower-tail probabilities at selected points
p_lo <- finitization::pbinom(n, p, N, val = c(2, 4, 5))
p_lo

# Upper-tail probabilities
p_hi <- finitization::pbinom(n, p, N, val = c(2, 4, 5), lower.tail = FALSE)
p_hi

## -----------------------------------------------------------------------------
# Median and 90th percentile
q_med_90 <- finitization::qbinom(n, p, N, prob = c(0.5, 0.9))
q_med_90

# Upper-tail quantile
q_upper10 <- finitization::qbinom(n, p, N, prob = 0.1, lower.tail = FALSE)
q_upper10

## -----------------------------------------------------------------------------
set.seed(123)
no <- 5e4
r <- finitization::rbinom(n, p, N, no)

# Compare empirical frequencies with the theoretical PMF
freq <- table(factor(r, levels = x)) / no
cbind(x = x, pmf = pmf_tbl$prob, emp = as.numeric(freq))[1:6, ]

# Visual comparison of theoretical and empirical frequencies
plot(x, pmf_tbl$prob, type = "h", lwd = 2,
     main = "Finitized Binomial: PMF vs empirical frequencies",
     xlab = "x", ylab = "Probability")
points(x, as.numeric(freq), pch = 16)
legend("topright", bty = "n",
       legend = c("PMF", "Empirical"),
       pch = c(NA, 16), lty = c(1, NA))


## -----------------------------------------------------------------------------
mfps <- finitization::getBinomialMFPS(n, N)
mfps

## -----------------------------------------------------------------------------
printFinitizedBinomialDensity(n, N)
printFinitizedBinomialDensity(n, N, val = c(0, 1, 2))
printFinitizedBinomialDensity(n, N, latex = TRUE)

## -----------------------------------------------------------------------------
pmf <- finitization::dbinom(n, p, N)
cdf <- finitization::pbinom(n, p, N)

# 1) Normalization
norm_err <- abs(sum(pmf$prob) - 1)

# 2) CDF consistency
cdf_from_pmf <- cumsum(pmf$prob)
cdf_diff_max <- max(abs(cdf_from_pmf - cdf$cdf))

# 3) Monotonicity
is_monotone <- all(diff(cdf$cdf) >= -1e-12)

# 4) Quantile inversion check
p_vec <- c(0.1, 0.25, 0.5, 0.9)
q_inv <- finitization::qbinom(n, p, N, prob = p_vec)
check_q_ok <- mapply(function(u, qx) {
  lhs <- cdf$cdf[qx + 1L]
  rhs <- if (qx > 0) cdf$cdf[qx] else 0
  (lhs + 1e-12 >= u) && (rhs + 1e-12 < u)
}, p_vec, q_inv)

# 5) Simulation vs. PMF moments
set.seed(456)
r_samp <- finitization::rbinom(n, p, N, 1e5)
mu_emp <- mean(r_samp)
var_emp <- var(r_samp)

mu_th <- sum(x * pmf$prob)
var_th <- sum((x - mu_th)^2 * pmf$prob)

list(norm_err = norm_err,
     cdf_diff_max = cdf_diff_max,
     monotone = is_monotone,
     quantile_check = check_q_ok,
     mean_theoretical = mu_th, mean_empirical = mu_emp,
     var_theoretical = var_th, var_empirical = var_emp)

## -----------------------------------------------------------------------------
library(finitization)

# Parameters used in the examples
n     <- 5      # finitization order -> support {0, 1, ..., 5}
theta <- 0.7    # Poisson mean (ensure theta lies within the MFPS for this n)
x     <- 0:n

## -----------------------------------------------------------------------------
# Full PMF table over 0:n
pmf_tbl <- finitization::dpois(n, theta)
head(pmf_tbl)

# PMF at selected points
d_points <- finitization::dpois(n, theta, val = c(0, 2, 5))
d_points

# Log-PMF for numerical work
d_log <- finitization::dpois(n, theta, val = 0:3, log = TRUE)
d_log

## -----------------------------------------------------------------------------
# Full CDF table
cdf_tbl <- ppois(n, theta)
head(cdf_tbl)

# Selected points (lower and upper tails)
p_lo <- ppois(n, theta, val = c(1, 3, 5))
p_hi <- ppois(n, theta, val = c(1, 3, 5), lower.tail = FALSE)
p_lo; p_hi

## -----------------------------------------------------------------------------
# Median and 90th percentile
q_med_90 <- qpois(n, theta, p = c(0.5, 0.9))
q_med_90

# Upper-tail quantile example
q_upper10 <- qpois(n, theta, p = 0.1, lower.tail = FALSE)
q_upper10

## -----------------------------------------------------------------------------
set.seed(2025)
no <- 5e4
r  <- rpois(n, theta, no)

# Empirical vs. theoretical frequencies (first rows)
freq <- table(factor(r, levels = x)) / no
cbind(x = x, pmf = pmf_tbl$prob, emp = as.numeric(freq))[1:6, ]
# Quick visual check
plot(x, pmf_tbl$prob, type = "h", lwd = 2,
     main = "Finitized Poisson: PMF vs empirical frequencies",
     xlab = "x", ylab = "Probability")
points(x, as.numeric(freq), pch = 16)
legend("topright", bty = "n",
       legend = c("PMF", "Empirical"),
       pch = c(NA, 16), lty = c(1, NA))

## -----------------------------------------------------------------------------
getPoissonMFPS(n)

## -----------------------------------------------------------------------------
printFinitizedPoissonDensity(n)
printFinitizedPoissonDensity(n, val = c(0, 1, 2))
printFinitizedPoissonDensity(n, latex = TRUE)

## -----------------------------------------------------------------------------
# Assumes 'n', 'theta', and x <- 0:n are already defined (see the Poisson section header).
# If not, uncomment the next three lines:
# n     <- 5
# theta <- 0.7
# x     <- 0:n

# --- 1) Finitized PMF/CDF ---
pmf_fin <- dpois(n, theta)    # data.frame: columns 'val', 'prob' (or log-prob if log=TRUE)
cdf_fin <- ppois(n, theta)    # data.frame: columns 'val', 'cdf'  (or log-cdf if log.p=TRUE)

# Normalization (PMF sums to 1 up to rounding)
norm_err <- abs(sum(pmf_fin$prob) - 1)

# CDF = cumulative sum of PMF (within numerical tolerance)
cdf_from_pmf <- cumsum(pmf_fin$prob)
cdf_diff_max <- max(abs(cdf_from_pmf - cdf_fin$cdf))

# CDF monotonicity
is_monotone <- all(diff(cdf_fin$cdf) >= -1e-12)

list(
  normalization_error = norm_err,
  cdf_max_abs_diff    = cdf_diff_max,
  cdf_monotone        = is_monotone
)

# --- 2) Quantile inversion accuracy ---
# For each p, q := qpois(n, theta, p) should satisfy: F(q) >= p and F(q-1) < p
p_vec <- c(0.01, 0.10, 0.25, 0.5, 0.9, 0.99)
q_inv <- qpois(n, theta, p = p_vec)

# Helper to get F(x) from the finitized CDF table (values start at 0, so index = x+1)
Ffin <- function(qx) if (qx < 0) 0 else cdf_fin$cdf[qx + 1L]

qinversion_ok <- mapply(function(p, qx) {
  (Ffin(qx) + 1e-12 >= p) && (Ffin(qx - 1L) + 1e-12 < p)
}, p_vec, q_inv)

data.frame(p = p_vec, q = q_inv, inversion_ok = qinversion_ok)

# --- 3) Moment preservation vs. classical Poisson ---
# We compare the first n raw moments of the finitized Poisson to the parent Poisson($\\theta$).
# Parent moments are computed by summation over {0,...,M} with negligible tail (< 1e-14).

# Finitized raw moments on {0..n}
raw_moment <- function(xx, pp, r) sum((xx^r) * pp)
mom_fin <- sapply(1:n, function(r) raw_moment(x, pmf_fin$prob, r))

# Choose M so that tail <= 1e-14 for the classical Poisson
M <- max(50L, ceiling(theta + 10*sqrt(theta))) # start with a reasonable guess
repeat {
  tail <- 1 - stats::ppois(M, lambda = theta)  # NOTE: explicit 'stats::' to avoid masking
  if (tail < 1e-14) break
  M <- M * 2L
  if (M > 1e6) stop("Grid grew too large while bounding the tail.")
}
grid <- 0:M
p_parent <- stats::dpois(grid, lambda = theta)  # classical Poisson PMF on {0..M}

# Parent raw moments (virtually exact with tiny tail)
mom_par <- sapply(1:n, function(r) raw_moment(grid, p_parent, r))

mom_cmp <- data.frame(
  r         = 1:n,
  finitized = mom_fin,
  parent    = mom_par,
  diff      = mom_fin - mom_par
)
mom_cmp

# --- 4) Simulation sanity check ---
# Samples from finitized Poisson should exhibit moments close to the finitized-theoretical ones.
set.seed(2025)
no <- 1e5
rs <- rpois(n, theta, no)

mean_emp <- mean(rs)
var_emp  <- var(rs)

mean_fin <- sum(x * pmf_fin$prob)
var_fin  <- sum( (x - mean_fin)^2 * pmf_fin$prob )

data.frame(
  quantity   = c("mean", "variance"),
  empirical  = c(mean_emp, var_emp),
  finitized  = c(mean_fin, var_fin),
  abs_diff   = c(abs(mean_emp - mean_fin), abs(var_emp - var_fin))
)

## -----------------------------------------------------------------------------
library(finitization)

# Parameters used in the examples
n      <- 3      # finitization order -> support {0, 1, 2, 3}
theta  <- 0.16   # log-series parameter (0 < theta < 1); ensure within MFPS for 'n'
x      <- 0:n

## -----------------------------------------------------------------------------
# Full PMF table over 0:n
pmf_tbl <- finitization::dlog(n, theta)
head(pmf_tbl)

# PMF at selected points
d_points <- finitization::dlog(n, theta, val = c(0, 2, 3))
d_points

# Log-PMF for numerical work
d_log <- finitization::dlog(n, theta, val = 0:3, log = TRUE)
d_log

## -----------------------------------------------------------------------------
# Full CDF table
cdf_tbl <- finitization::plog(n, theta)
head(cdf_tbl)

# Selected points (lower and upper tails)
p_lo <- finitization::plog(n, theta, val = c(1, 3))
p_hi <- finitization::plog(n, theta, val = c(1, 3), lower.tail = FALSE)
p_lo; p_hi

## -----------------------------------------------------------------------------
# Median and 90th percentile
q_med_90 <- finitization::qlog(n, theta, p = c(0.5, 0.9))
q_med_90

# Upper-tail quantile example
q_upper10 <- finitization::qlog(n, theta, p = 0.1, lower.tail = FALSE)
q_upper10

## -----------------------------------------------------------------------------
set.seed(2025)
no <- 5e6
r  <- finitization::rlog(n, theta, no)

# Empirical vs. theoretical frequencies (first rows)
freq <- table(factor(r, levels = x)) / no
cbind(x = x, pmf = pmf_tbl$prob, emp = as.numeric(freq))[1:4, ]

## -----------------------------------------------------------------------------
# Quick visual check
plot(x, pmf_tbl$prob, type = "h", lwd = 2,
     main = "Finitized Logarithmic: PMF vs empirical frequencies",
     xlab = "x", ylab = "Probability")
points(x, as.numeric(freq), pch = 16)
legend("topright", bty = "n",
       legend = c("PMF", "Empirical"),
       pch = c(NA, 16), lty = c(1, NA))

## -----------------------------------------------------------------------------
getLogarithmicMFPS(n)

## -----------------------------------------------------------------------------
printFinitizedLogarithmicDensity(n, val = c(0))

## -----------------------------------------------------------------------------
# --- Finitized raw moments on {0..n} ---
pmf_fin_tbl <- finitization::dlog(n, theta)   # expected: data.frame with columns 'val', 'prob'
stopifnot(is.data.frame(pmf_fin_tbl), all(c("val","prob") %in% names(pmf_fin_tbl)))
stopifnot(all(pmf_fin_tbl$val == x))
stopifnot(abs(sum(pmf_fin_tbl$prob) - 1) < 1e-12)

pmf_fin <- pmf_fin_tbl$prob
head(pmf_fin_tbl)



## -----------------------------------------------------------------------------
# Parent pmf implied by the given MGF (zero-including log-series)
dlog_parent0 <- function(x, theta) {
  -1 / log(1 - theta) * theta^(x + 1) / (x + 1)
}

# Choose M until the remaining tail mass is < 1e-14
M <- 200L
repeat {
  s <- sum(dlog_parent0(0:M, theta))
  tail <- 1 - s
  if (tail < 1e-14) break
  M <- M * 2L
  if (M > 1e7) stop("Grid grew too large while bounding the tail.")
}
grid  <- 0:M
p_par <- dlog_parent0(grid, theta)
stopifnot(abs(sum(p_par) - 1) < 1e-10)

c(head(data.frame(x = grid, p = p_par), 6),
  tail_mass = 1 - sum(p_par))

raw_moment <- function(xx, pp, r) sum( (xx^r) * pp )

mom_fin <- sapply(1:n, function(r) raw_moment(x,    pmf_fin, r))
mom_par <- sapply(1:n, function(r) raw_moment(grid, p_par,    r))

mom_cmp <- data.frame(
  r         = 1:n,
  finitized = mom_fin,
  parent    = mom_par,
  diff      = mom_fin - mom_par
)
mom_cmp


## -----------------------------------------------------------------------------
pmf <- finitization::dlog(n, theta)
cdf <- finitization::plog(n, theta)

# Normalization
norm_err <- abs(sum(pmf$prob) - 1)

# CDF consistency and monotonicity
cdf_from_pmf <- cumsum(pmf$prob)
cdf_diff_max <- max(abs(cdf_from_pmf - cdf$cdf))
is_monotone  <- all(diff(cdf$cdf) >= -1e-12)

# Quantile inversion check
p_vec <- c(0.01, 0.1, 0.25, 0.5, 0.9, 0.99)
q_inv <- finitization::qlog(n, theta, p = p_vec)
Ffin  <- function(qx) if (qx < 0) 0 else cdf$cdf[qx + 1L]
qinversion_ok <- mapply(function(p, qx) {
  (Ffin(qx) + 1e-12 >= p) && (Ffin(qx - 1L) + 1e-12 < p)
}, p_vec, q_inv)

list(
  normalization_error = norm_err,
  cdf_max_abs_diff    = cdf_diff_max,
  cdf_monotone        = is_monotone,
  quantile_inversion  = qinversion_ok
)

## -----------------------------------------------------------------------------
# ---- Timings for random variates  ----

# Workload
no      <- 1e6      # draws per generator call
batches <- 5L       # average over this many batches
target  <- 0.05     # >= 50 ms per batch for stable timing

# Optional parent for Logarithmic (extraDistr::rlgser)
have_extraDistr <- requireNamespace("extraDistr", quietly = TRUE)

# Auto-scale inner loop so each batch is long enough; return sec per one call
avg_time_per_call <- function(fun, b = batches, t = target) {
  reps <- 1L
  invisible(fun())  # warm-up
  repeat {
    t0 <- proc.time()[["elapsed"]]
    for (b in seq_len(b)) {
      for (r in seq_len(reps)) fun()
    }
    dt <- proc.time()[["elapsed"]] - t0
    if ((dt / b) >= t || reps >= 1e5L) {
      return((dt / b) / reps)
    }
    reps <- reps * 10L
  }
}

# Parameters
pars <- list(
  pois = list(n = 3L, theta = 0.10),
  nbin = list(n = 3L, q = 0.10, k = 5L),      # parent: size = k, prob = 1 - q
  bin  = list(n = 3L, p = 0.10, N = 10L),
  log  = list(n = 3L, theta = 0.10)           # parent via extraDistr::rlgser() - 1
)

# One-call closures (each call draws 'no' variates)
fun_fin_pois <- function() finitization::rpois(pars$pois$n, pars$pois$theta, no)
fun_par_pois <- function() stats::rpois(no, lambda = pars$pois$theta)

fun_fin_nbin <- function() finitization::rnegbinom(pars$nbin$n, pars$nbin$q, pars$nbin$k, no)
fun_par_nbin <- function() stats::rnbinom(no, size = pars$nbin$k, prob = 1 - pars$nbin$q)

fun_fin_bin  <- function() finitization::rbinom(pars$bin$n, pars$bin$p, pars$bin$N, no)
fun_par_bin  <- function() stats::rbinom(no, size = pars$bin$N, prob = pars$bin$p)

fun_fin_log  <- function() finitization::rlog(pars$log$n, pars$log$theta, no)
fun_par_log  <- if (have_extraDistr) function() extraDistr::rlgser(no, pars$log$theta) - 1L else NULL

# Measure (pass batches/target explicitly if you want)
t_fin_pois <- avg_time_per_call(fun_fin_pois, b = batches, t = target)
t_par_pois <- avg_time_per_call(fun_par_pois, b = batches, t = target)

t_fin_nbin <- avg_time_per_call(fun_fin_nbin, b = batches, t = target)
t_par_nbin <- avg_time_per_call(fun_par_nbin, b = batches, t = target)

t_fin_bin  <- avg_time_per_call(fun_fin_bin,  b = batches, t = target)
t_par_bin  <- avg_time_per_call(fun_par_bin,  b = batches, t = target)

t_fin_log  <- avg_time_per_call(fun_fin_log,  b = batches, t = target)
t_par_log  <- if (!is.null(fun_par_log)) avg_time_per_call(fun_par_log, b = batches, t = target) else NA_real_

# Speedup = parent / finitized (how many times faster finitized is)
res <- data.frame(
  Family  = c("Poisson", "NegBin", "Binomial", "Logarithmic"),
  Speedup = c(
    t_par_pois / t_fin_pois,
    t_par_nbin / t_fin_nbin,
    t_par_bin  / t_fin_bin,
    t_par_log  / t_fin_log
  ),
  check.names = FALSE
)

cat(
  sprintf("%-12s %s\n", "Family", "Speedup (finitized versus parent)"),
  paste(
    sprintf("%-12s %22.14f", res$Family, res$Speedup),
    collapse = "\n"
  ),
  sep = "\n"
)

