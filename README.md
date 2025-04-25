# finitization

<!-- badges: start -->
[![](https://github.com/bogdanoancea/finitization/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/bogdanoancea/finitization/actions/workflows/R-CMD-check.yml)
<!-- badges: end -->

The goal of this package is to implement the concept of probability distribution finitization.

---

## 🛠 Installation Instructions

### 🔹 Linux (Debian/Ubuntu)
Install dependencies via `apt`:

```bash
sudo apt-get update
sudo apt-get install -y libgmp-dev libcln-dev libginac-dev pkg-config
```

### 🔹 macOS (with Homebrew)
Install dependencies via Homebrew:

```bash
brew install gmp cln ginac pkg-config
```

> Ensure that Homebrew’s `bin` and `lib` paths are visible to the compiler (especially when using RStudio).

### 🔹 Windows
There is currently no automated installation for Windows. You need to:

1. Install **Rtools**: [https://cran.r-project.org/bin/windows/Rtools/](https://cran.r-project.org/bin/windows/Rtools/)
2. Manually install and build static versions of GMP, CLN, and GiNaC (e.g. via MSYS2 or build from source).
3. Ensure headers and libraries are discoverable via environment variables or placed where Rtools will find them.

> If you are unfamiliar with building C/C++ libraries on Windows, we recommend using WSL (Windows Subsystem for Linux) or building the package on Linux/macOS.

---

## 📦 Building the Package

Once dependencies are installed, you can build and install the package from source:

```bash
git clone https://github.com/bogdanoancea/finitization.git
cd finitization
R CMD build .
R CMD INSTALL finitization_0.1.0.tar.gz
```

Or use `devtools`:

```r
# install.packages("devtools")
devtools::install_github("bogdanoancea/finitization")
```

---

## 🔍 Example

This is a basic example which shows you how to solve a common problem:

```r
library(finitization)
## basic example code
```

---

## 🧪 Testing

To run tests:

```r
library(testthat)
testthat::test_package("finitization")
```

---

## 🔗 Useful Links
- GMP: https://gmplib.org/
- CLN: https://www.ginac.de/CLN/
- GiNaC: https://www.ginac.de/Download.html
- Rtools (Windows): https://cran.r-project.org/bin/windows/Rtools/

---

For any issues or feature requests, please use the [GitHub issue tracker](https://github.com/bogdanoancea/finitization/issues).

Maintainer: Bogdan Oancea <bogdan.oancea@gmail.com>

