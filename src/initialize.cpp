#include <Rcpp.h>
#include <iostream>
#include <sstream>  // use stringstream instead of file stream

// [[Rcpp::export]]
void initialize_finitization() {
    static std::ostringstream null_stream;
    std::cout.rdbuf(null_stream.rdbuf());
    std::cerr.rdbuf(null_stream.rdbuf());
}
