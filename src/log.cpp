#include <Rcpp.h>
#include <string>
#include <vector>


#include "FinitizedLogarithmicDistribution.h"


using namespace Rcpp;
using namespace std;


//' @param n The finitization order. It should be an integer > 0.
//' @param theta The parameter of the logarithmic distribution.
//' @param no The number of random values to be generated according to the finitized logarithmic distribution.
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



//' @param n The finitization order. It should be an integer > 0.
// [[Rcpp::export]]
String MFPS_log_pdf(int n) {
    if(n < 1)
        return NA_REAL;
    Finitization* f = new FinitizedLogarithmicDistribution(n, 0.01);
    string result = f->pdfToString(n-1);
    delete f;
    return String(result);
}


//' @param n The finitization order. It should be an integer > 0.
//' @param theta theta The parameter of the logarithmic distribution.
//' @param val  The value of the variable for which the string reprsentation of the probability density function is returned.
//' If NULL, this function returns the pdf for all possible values, i.e. {0 .. n}.
//' @param latex If TRUE, a string representation of the pdf formatted in Latex format is returned, otherwise it returns
//'  the string representation of the pdf as an R expression.
//' @export
// [[Rcpp::export]]
String c_printFinitizedLogarithmicDensity(int n, int val, bool latex = false) {
    Finitization* f = new FinitizedLogarithmicDistribution(n, 0.01);
    string result =  f->pdfToString(val, latex);
    Rcout << result << endl;
    delete f;
    return String(result);

}



//' @param n The finitization order. It should be an integer > 0.
//' @param val The value of the variable for which the probability density function is computed.
//' @param theta The parameter of the logarithmic distribution.
//' @export
//  [[Rcpp::export]]
double c_dlog(int n, double theta, double val) {
    Finitization* f = new FinitizedLogarithmicDistribution(n, theta);
    double p =  f->fin_pdf(val);
    delete f;
    return p;
}


