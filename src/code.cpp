#include <Rcpp.h>
#include <string>
#include "FinitizedPoissonDistribution.h"

using namespace Rcpp;
using namespace std;

//' @param n
//' @param theta
//' @param no
//' @export
// [[Rcpp::export]]
IntegerVector rpois(int n, double theta, unsigned no) {
    Finitization* f = new FinitizedPoissonDistribution(n, theta);
    IntegerVector r = f->rvalues(no);
    delete f;
    return r;
}

//' @param n
//' @param theta
//' @param val
//' @export
// [[Rcpp::export]]
double dpois(int n, double theta, double val) {
    if( val > n || val < 0) {
        return 0;
    }
    Finitization* f = new FinitizedPoissonDistribution(n, theta);
    double p =  f->getProb(val);
    delete f;
    return p;
}

//' @param n
//' @param theta
//' @param val
//' @export
// [[Rcpp::export]]
SEXP printFinitizedPoissonDensity(int n, int val, bool latex = false) {
    Finitization* f = new FinitizedPoissonDistribution(n, 1);
    string result =  f->pdfToString(val, latex);
    Rcout << result;
    return Rcpp::wrap(result);

}

//' @param n
//' @param theta
//' @param val
//' @export
// [[Rcpp::export]]
String printFinitizedPoissonDensity2(int n, int val) {
    Finitization* f = new FinitizedPoissonDistribution(n, 1);
    string result =  f->pdfToString(val);
    return String(result);
}
