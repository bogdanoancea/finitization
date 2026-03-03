# finitization

<!-- badges: start -->
[![](https://github.com/bogdanoancea/finitization/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/bogdanoancea/finitization/actions/workflows/R-CMD-check.yml)
[![finitization build status](https://bogdanoancea.r-universe.dev/badges/finitization)](https://bogdanoancea.r-universe.dev/finitization)
[![R-universe](https://bogdanoancea.r-universe.dev/badges/latest)](https://bogdanoancea.r-universe.dev)
[![License: GPL v3](https://img.shields.io/badge/license-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![R ≥ 4.2](https://img.shields.io/badge/R-%3E%3D%204.2-276DC3?logo=r&logoColor=white)](https://cran.r-project.org/)
[![Docs](https://img.shields.io/badge/docs-online-success)](https://bogdanoancea.r-universe.dev/finitization/doc)

<!-- badges: end -->

The goal of this package is to implement the concept of probability distribution 
finitization.

## Title

Moment-Preserving Finitization for Power-Series Distributions

## Description

The **finitization** package implements  the finitization of discrete 
probability distributions, a technique that approximates a distribution by 
preserving a finite number of moments. These finitized distributions enable 
faster random variate generation than inverse transform sampling and are 
useful in simulation and statistical modeling.
Symbolic computation is performed using the GiNaC library together with
CLN and GMP for multiprecision arithmetic.
The package requires compilation and links against external symbolic and
multiprecision libraries.

## Installation

### CRAN (Linux / macOS)

Required system libraries must be installed prior to installation.

Debian / Ubuntu:

    sudo apt-get update
    sudo apt-get install -y libgmp-dev libcln-dev libginac-dev pkg-config

macOS (Homebrew):

    brew install gmp cln ginac pkg-config

Then install from CRAN:

    install.packages("finitization")

## System Requirements

-   GMP
-   CLN
-   GiNaC
-   pkg-config
-   make
-   C++17-compatible compiler


##  Building the Package from source

Once dependencies are installed, you can build and install the package from source:

    git clone https://github.com/bogdanoancea/finitization.git
    cd finitization
    R CMD build .
    R CMD INSTALL finitization_0.1.0.tar.gz

Or use `devtools`:

    install.packages("devtools")
    devtools::install_github("bogdanoancea/finitization")

##  Testing

To run tests:

    library(testthat)
    testthat::test_package("finitization")

## Example

    library(finitization)

    dpois(4, 0.5, c(0,1,3))
    dpois(4, 0.5, c(0,1,3), log = TRUE)

## Note to CRAN Reviewers

This package depends on the external libraries GMP, CLN, and GiNaC for
symbolic differentiation and formal Taylor series expansion. These
libraries are required at compile time and are available on standard
Linux and macOS systems via system package managers.

CRAN Windows builders do not provide CLN or GiNaC by default. For this
reason, the package declares `OS_type: unix` in DESCRIPTION and is
intended for CRAN builds on Linux and macOS platforms only.

The Windows installation instructions provided in INSTALL.md are for
manual source builds outside CRAN infrastructure.

## License

GPL (\>= 3)

## 🔗 Useful Links
- GMP: https://gmplib.org/
- CLN: https://www.ginac.de/CLN/
- GiNaC: https://www.ginac.de/Download.html

## Bug Reports

https://github.com/bogdanoancea/finitization/issues

Maintainer: Bogdan Oancea <bogdan.oancea@gmail.com>
