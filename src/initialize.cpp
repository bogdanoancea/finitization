#include <Rcpp.h>
#include <iostream>
#include <fstream>

// Suppresses cout and cerr output
void silence_ginac_output() {
#ifdef _WIN32
    static std::ofstream null_stream("NUL");
    if (null_stream.is_open()) {
        std::cout.rdbuf(null_stream.rdbuf());
        std::cerr.rdbuf(null_stream.rdbuf());
    }
#else
    static std::ofstream null_stream("/dev/null");
    if (null_stream.is_open()) {
        std::cout.rdbuf(null_stream.rdbuf());
        std::cerr.rdbuf(null_stream.rdbuf());
    }
#endif
}

// Exported function to R
// [[Rcpp::export]]
void initialize_finitization() {
    silence_ginac_output();
}
