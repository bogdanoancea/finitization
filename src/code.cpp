#include <Rcpp.h>
#include <string>
#include <vector>

#include "FinitizedPoissonDistribution.h"
#include "FinitizedLogarithmicDistribution.h"

using namespace Rcpp;
using namespace std;

//' @param n
//' @param theta
//' @param no
//' @export
// [[Rcpp::export]]
IntegerVector rpois(int n, double theta, unsigned no) {
    IntegerVector r(no);
    if( theta <= 0)
        for( int i = 0; i <= no; ++i)
            r[i] = NA_INTEGER;
    else {
        Finitization* f = new FinitizedPoissonDistribution(n, theta);
        r = f->rvalues(no);
        delete f;
    }
    return r;
}

//' @param n
//' @param theta
//' @param val
//' @export
//  [[Rcpp::export]]
double c_dpois(int n, double theta, double val) {
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
String c_printFinitizedPoissonDensity(int n, int val, bool latex = false) {
    Finitization* f = new FinitizedPoissonDistribution(n, 1);
    string result =  f->pdfToString(val, latex);
    //if(latex)
    Rcout << result << endl;

    return String(result);

}


//' @param n
//' @export
// [[Rcpp::export]]
double getPoissonMFPSUL(int n) {
    if(n < 1)
        return NA_REAL;
    Finitization* f = new FinitizedPoissonDistribution(n, 1);
    double result = f->getMFPSUL();
    delete f;
    return result;
}


//' @param n
// [[Rcpp::export]]
String MFPS_log_pdf(int n) {
    if(n < 1)
        return NA_REAL;
    Finitization* f = new FinitizedLogarithmicDistribution(n, 0.01);
    string result = f->pdfToString(n-1);
    delete f;
    return String(result);
}

//' @param n
//' @param theta
//' @param no
//' @export
// [[Rcpp::export]]
IntegerVector rlog(int n, double theta, unsigned no) {
    IntegerVector r(no);
    if( theta <= 0)
        for( int i = 0; i <= no; ++i)
            r[i] = NA_INTEGER;
    else {
        Finitization* f = new FinitizedLogarithmicDistribution(n, theta);
        r = f->rvalues(no);
        delete f;
    }
    return r;
}


//' @param n
//' @param theta
//' @param val
//' @export
// [[Rcpp::export]]
String c_printFinitizedLogarithmicDensity(int n, int val, bool latex = false) {
    Finitization* f = new FinitizedLogarithmicDistribution(n, 0.01);
    string result =  f->pdfToString(val, latex);
    //if(latex)
    Rcout << result << endl;

    return String(result);

}


//' @param n
//' @param theta
//' @param val
//' @export
//  [[Rcpp::export]]
double c_dlog(int n, double theta, double val) {
    Finitization* f = new FinitizedLogarithmicDistribution(n, theta);
    double p =  f->getProb(val);
    delete f;
    return p;
}
