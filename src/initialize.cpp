#include <Rcpp.h>
#include <iostream>
#include <fstream>

// Anonymous namespace (local to file)
namespace {
#ifdef _WIN32
std::ofstream null_stream("NUL");
#else
    std::ofstream null_stream("/dev/null");
#endif
}

// Suppresses cout and cerr output
void silence_ginac_output() {
#ifdef _WIN32
    std::ofstream null_stream_win("NUL");
    std::cout.rdbuf(null_stream_win.rdbuf());
    std::cerr.rdbuf(null_stream_win.rdbuf());
#else
    std::cout.rdbuf(null_stream.rdbuf());
    std::cerr.rdbuf(null_stream.rdbuf());
#endif
}

// Exported function to R
// [[Rcpp::export]]
void initialize_finitization() {
    silence_ginac_output();
}
