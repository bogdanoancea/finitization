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

### 🔹 Windows  (RTools44 + MSYS2)
1. **Install RTools44**  
   Download and run the installer from  
   https://cran.r-project.org/bin/windows/Rtools/

2. **Open the “MSYS2 UCRT 64‑bit” shell** 
   Supposing that RTools44 is intalled in "C:\rtools44" then double click on "C:\rtools44\ucrt64.exe". 
   
3. **Fully update MSYS2, update the package database and install the toolchain + pkgconf**  
   ```bash
    pacman -Syu        # if it asks to close/reopen, do it, then run again:
    pacman -Syu
    pacman -S --needed mingw-w64-ucrt-x86_64-toolchain mingw-w64-ucrt-x86_64-pkgconf
   ```

4. **Install GMP**  
   ```bash
   pacman -S --needed mingw-w64-ucrt-x86_64-gmp
   ```

5. **Download, compile and install CLN and GiNaC**  
   CLN and GiNaC are not available via pacman; follow the instructions on their websites:  
   - **CLN (Class Library for Numbers):** https://www.ginac.de/cln  
   - **GiNaC (GiNaC is Not a CAS):** https://www.ginac.de  

   Example (adjust versions as needed):  
   ```bash
   # CLN
   pacman -S --needed wget
   wget https://www.ginac.de/CLN/cln-1.3.7.tar.bz2
   tar xf cln-1.3.7.tar.bz2
   cd cln-1.3.7
   ./configure --prefix=/ucrt64
   make
   make install
   cd ..

   # GiNaC
   wget https://www.ginac.de/ginac-1.8.9.tar.bz2
   tar xf ginac-1.8.9.tar.bz2
   cd ginac-1.8.9
   ./configure --prefix=/ucrt64
   make
   make install
   ```

6. **Make sure R sees the compilers**  
   In your R session (or add to your `~/.Renviron`):
   ```r
   Sys.setenv(PATH = paste(
     "C:/rtools44/ucrt64/bin",
     Sys.getenv("PATH"),
     sep = ";"
   ))
   ```

7. **Build & install the package**  
   From your package root (in a cmd prompt or MSYS2 UCRT64 shell):
   ```bash
   R CMD build .
   R CMD INSTALL finitization_*.tar.gz
   ```

> Once the toolchain, `pkgconf`, GMP headers & libraries, and CLN/GiNaC are installed, you can install **finitization** exactly as on Linux or macOS.  
> If you encounter missing header or symbol errors, confirm that `/ucrt64/include` and `/ucrt64/lib` are on your compiler’s search path.  
> You may also use `devtools::install()` from within R for convenience.

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

