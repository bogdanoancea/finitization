#include <Rcpp.h>
#include <string>
#include <vector>


#include "FinitizedLogarithmicDistribution.h"
#include "FinitizedPoissonDistribution.h"
#include "FinitizedBinomialDistribution.h"
#include "FinitizedNegativeBinomialDistribution.h"
#include "DistributionType.h"

using namespace Rcpp;
using namespace std;

//' @param n The finitization order. It should be an integer > 0.
//' @param val The value of the variable for which the probability density function is computed.
//' @param params Other parameters of the distribution.
//' @param dtype The type of the distribution: Poisson, Binomial, NegativeBinomial, Logaritmic
//' @param latex If true it returns a Latex formatted string representation of the pdf,otherwise it returns
//' the string representation of the pdf as an R expression.
// [[Rcpp::export]]
String c_printDensity(int n, int val, Rcpp::List const &params, int dtype, bool latex = false) {

    Finitization* f = nullptr;
    string result;
    NumericVector N(1);
    NumericVector k(1);

    switch(dtype) {
    case DistributionType::POISSON:
        f = new FinitizedPoissonDistribution(n, 0.1);
        break;
    case DistributionType::LOGARITHMIC:
        f = new FinitizedLogarithmicDistribution(n, 0.01);
        break;
    case DistributionType::BINOMIAL:
        if(params.containsElementNamed("N")) {
            N[0] = Rcpp::as < int >( params["N"]);
            f = new FinitizedBinomialDistribution(n, 0, N[0]);
        }
        else
            Rcerr << "Parameter N of the Binomial distribution not provided!" << endl;
        break;

    case DistributionType::NEGATIVEBINOMIAL:
        if(params.containsElementNamed("k")) {
            k[0] = Rcpp::as < int >( params["k"]);
            f = new FinitizedNegativeBinomialDistribution(n, 0, k[0]);
        }
        else
            Rcerr << "Parameter k of the Negative Binomial distribution not provided!" << endl;
        break;
    default:
        Rcerr << " Distribution type unsupported!" << endl;
    }

    if(f) {
        result =  f->pdfToString(val, latex);
        Rcout << result << endl;
        delete f;
    }
    return String(result);

}

// [[Rcpp::export]]
int getPoissonType() {
    return DistributionType::POISSON;
}

// [[Rcpp::export]]
int getNegativeBinomialType() {
    return DistributionType::NEGATIVEBINOMIAL;
}

// [[Rcpp::export]]
int getBinomialType() {
    return DistributionType::BINOMIAL;
}

// [[Rcpp::export]]
int getLogarithmicType() {
    return DistributionType::LOGARITHMIC;
}

