#include <Rcpp.h>
#include <vector>
#include "FinitizedPoissonDistribution.h"
using namespace Rcpp;


//' @param n
//' @param theta
//' @param no
//' @export
// [[Rcpp::export]]
IntegerVector generatePoisson(int n, double theta, unsigned no) {

    FinitizedDistribution* poisson = new FinitizedPoissonDistribution(n, theta);
    vector<int> rndValues = poisson->rvalues(no);
    IntegerVector result(no);
    for(int i = 0; i < rndValues.size(); i++) {
        //cout << i << " " << rndValues[i] << endl;
        result[i] = rndValues[i];
    }
    return result;
}


//' Multiply a number by two
//'
//' @param x A single integer.
//' @export
// [[Rcpp::export]]
int timesTwo(int x) {
    return x * 2;
}
