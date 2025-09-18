## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4
)

## -----------------------------------------------------------------------------
library(finitization)

## -----------------------------------------------------------------------------
set.seed(123)
#sample <- rbinom(n = 5, p = 0.4, order = 3, size = 1000)
#hist(sample, breaks = 20, main = "Finitized Binomial Sample", col = "lightblue")

## -----------------------------------------------------------------------------
standard <- stats::rbinom(1000, size = 5, prob = 0.4)
hist(standard, breaks = 20, main = "Standard Binomial Sample", col = "salmon")

## -----------------------------------------------------------------------------
#mean(sample)
#var(sample)

## -----------------------------------------------------------------------------
#plot(density(standard), col = "red", lwd = 2, main = "Density Comparison")
#lines(density(sample), col = "blue", lwd = 2)
#legend("topright", legend = c("Standard", "Finitized"), col = c("red", "blue"), lwd = 2)

