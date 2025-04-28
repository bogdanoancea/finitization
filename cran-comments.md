## NOTE (for CRAN)
If the corresponding dynamic libraries are not installed on the system, this package will
statically link to GiNaC, CLN, and GMP; their internal use of `std::cout`, `std::cerr`,
`abort()` and `rand()` is harmless and does not interfere with R’s I/O or RNG.
