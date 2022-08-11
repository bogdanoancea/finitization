#include <Rcpp.h>
#include <string>
#include <vector>


#include "FinitizedNegativeBinomialDistribution.h"
using namespace Rcpp;
using namespace std;


//' @param n The finitization order. It should be an integer > 0.
//' @param p The parameter of the Binomial distribution: the sucess probability for each trial.
//' @param N The number of trials.
//' @param no The number of the random values to be generated according to the finitized Binomial distribution.
//' @export
// [[Rcpp::export]]
IntegerVector rnegbinom(int n, double p, int k, unsigned no) {
    IntegerVector r(no);
    if( p <= 0 || k <= 0)
        for( int i = 0; i <= no; ++i)
            r[i] = NA_INTEGER;
    else {
        Finitization* f = new FinitizedNegativeBinomialDistribution(n, p, k);
        r = f->rvalues(no);
        delete f;
    }
    return r;
}


//' @param n The finitization order. It should be an integer > 0.
//' @param N The number of trials.
// [[Rcpp::export]]
String MFPS_negbinom_pdf(int n, int k) {
    if(n < 1 || k < 1 )
        return NA_REAL;

    Finitization* f = new FinitizedNegativeBinomialDistribution(n, 0.1, k);
    string result = f->pdfToString(n-1);
    delete f;
    return String(result);
}



//' @param n The finitization order. It should be an integer > 0.
//' @param p The parameter of the binomial distribution, i.e. the probability of success.
//' @param N The number of trials.
//' @param val  The value of the variable for which the string representation of the probability density function is returned.
//' If NULL, this function returns the pdf for all possible values, i.e. {0 .. n}.
//' @param latex If TRUE, a string representation of the pdf formatted in Latex format is returned, otherwise it returns
//'  the string representation of the pdf as an R expression.
//' @export
// [[Rcpp::export]]
String c_printFinitizedNegativeBinomialDensity(int n, int k, int val, bool latex = false) {
    Finitization* f = new FinitizedNegativeBinomialDistribution(n, 0.1, k);
    string result =  f->pdfToString(val, latex);
    Rcout << result << endl;
    delete f;
    return String(result);
}



//' @param n The finitization order. It should be an integer > 0.
//' @param N The number of trials.
//' @param val The value of the variable for which the probability density function is computed.
//' @param p The parameter of the Binomial distribution: the success probability for each trial.
//' @export
//  [[Rcpp::export]]
double c_dnegbinom(int n, double p, int k, double val) {
    Finitization* f = new FinitizedNegativeBinomialDistribution(n, p, k);
    double pp =  f->fin_pdf(val);
    delete f;
    return pp;
}
