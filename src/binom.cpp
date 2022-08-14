#include <Rcpp.h>
#include <string>
#include <vector>


#include "FinitizedBinomialDistribution.h"
using namespace Rcpp;
using namespace std;


//' @param n The finitization order. It should be an integer > 0.
//' @param p The parameter of the Binomial distribution: the sucess probability for each trial.
//' @param N The number of trials.
//' @param no The number of the random values to be generated according to the finitized Binomial distribution.
//' @export
// [[Rcpp::export]]
IntegerVector rbinom(int n, double p, int N, unsigned no) {
    IntegerVector r(no);
    if( p <= 0 || N <= 0)
        for( int i = 0; i <= no; ++i)
            r[i] = NA_INTEGER;
    else {
        Finitization* f = new FinitizedBinomialDistribution(n, p, N);
        r = f->rvalues(no);
        delete f;
    }
    return r;
}


//' @param n The finitization order. It should be an integer > 0.
//' @param N The number of trials.
// [[Rcpp::export]]
String MFPS_binom_pdf(int n, int N) {
    if(n < 1 || N < 1 )
        return NA_REAL;

    Finitization* f = new FinitizedBinomialDistribution(n, 0.1, N);
    string result = f->pdfToString(n-1);
    delete f;
    return String(result);
}




//' @param n The finitization order. It should be an integer > 0.
//' @param N The number of trials.
//' @param val The value of the variable for which the probability density function is computed.
//' @param p The parameter of the Binomial distribution: the success probability for each trial.
//' @export
//  [[Rcpp::export]]
double c_dbinom(int n, double p, int N, double val) {
    Finitization* f = new FinitizedBinomialDistribution(n, p, N);
    double pp =  f->fin_pdf(val);
    delete f;
    return pp;
}
