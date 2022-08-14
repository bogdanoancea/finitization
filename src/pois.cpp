#include <Rcpp.h>
#include <string>
#include <vector>

#include "FinitizedPoissonDistribution.h"
#include "DistributionType.h"

using namespace Rcpp;
using namespace std;


//' @param n The finitization order. It should be an integer > 0.
//' @param theta The parameter of the Poisson distribution.
//' @param no The number of the random values to be generated according to the finitized Poisson distribution.
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


//' @param n The finitization order. It should be an integer > 0.
// [[Rcpp::export]]
String MFPS_pois_pdf(int n) {
    if(n < 1)
        return NA_REAL;
    Finitization* f = new FinitizedPoissonDistribution(n, 0.01);
    string result = f->pdfToString(n-1);
    delete f;
    return String(result);
}
