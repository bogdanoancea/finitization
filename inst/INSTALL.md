# INSTALLATION GUIDE

This document provides full build instructions for source installations
and non-CRAN environments.

The package requires:

-   GMP (GNU Multiple Precision Arithmetic Library)
-   CLN (Class Library for Numbers)
-   GiNaC (symbolic C++ library)
-   pkg-config
-   C++17-compatible compiler

  -------------------------
  Linux (Debian / Ubuntu)
  -------------------------

    sudo apt-get update
    sudo apt-get install -y libgmp-dev libcln-dev libginac-dev pkg-config

Build and install:

    R CMD build finitization
    R CMD INSTALL finitization_*.tar.gz

  ------------------
  macOS (Homebrew)
  ------------------

    brew install gmp cln ginac pkg-config

If needed:

    export PATH="/opt/homebrew/bin:$PATH"
    export PKG_CONFIG_PATH="/opt/homebrew/lib/pkgconfig:$PKG_CONFIG_PATH"

Build as usual:

    R CMD build finitization
    R CMD INSTALL finitization_*.tar.gz

  -----------------------------------------------
  Windows (Manual Source Build -- Outside CRAN)
  -----------------------------------------------

CRAN Windows builders do not provide CLN or GiNaC. Manual compilation is
required.

1.  Install Rtools (UCRT toolchain).
2.  Open the MSYS2 UCRT64 shell.

Install required packages:

    pacman -Syu
    pacman -S --needed mingw-w64-ucrt-x86_64-toolchain
    pacman -S --needed mingw-w64-ucrt-x86_64-pkgconf
    pacman -S --needed mingw-w64-ucrt-x86_64-gmp

Download and compile CLN:

    wget https://www.ginac.de/CLN/cln-1.3.7.tar.bz2
    tar xf cln-1.3.7.tar.bz2
    cd cln-1.3.7
    ./configure --prefix=/ucrt64
    make
    make install

Download and compile GiNaC:

    wget https://www.ginac.de/ginac-1.8.9.tar.bz2
    tar xf ginac-1.8.9.tar.bz2
    cd ginac-1.8.9
    ./configure --prefix=/ucrt64
    make
    make install

Ensure that /ucrt64/include and /ucrt64/lib are visible to the compiler.

Build and install:

    R CMD build finitization
    R CMD INSTALL finitization_*.tar.gz

------------------------------------------------------------------------

Development installation:

    devtools::install_github("bogdanoancea/finitization")
