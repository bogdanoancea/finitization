## NOTE (for CRAN)
If the corresponding dynamic libraries are not installed on the system, this package will
statically link to GiNaC, CLN, and GMP.
The NOTE regarding std::cout and std::cerr originates from the output redirection code in the package initialization (silencing GiNaC library output). The package itself does not actively use console output.
