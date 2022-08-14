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
StringVector c_printDensity(int n, IntegerVector val, Rcpp::List const &params, int dtype, bool latex = false) {

    Finitization* f = nullptr;
    StringVector result(val.size());
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
        for(int i = 0; i < val.size(); ++i) {
            result[i] =  f->pdfToString(val[i], latex);
            //Rcout << result[i] << endl;
        }
        delete f;
    }
    return result;

}

//' @param n The finitization order. It should be an integer > 0.
//' @param val The value of the variable for which the probability density function is computed.
//' @param params Other parameters of the distribution.
//' @param dtype The type of the distribution: Poisson, Binomial, NegativeBinomial, Logaritmic
//' @param latex If true it returns a Latex formatted string representation of the pdf,otherwise it returns
//' the string representation of the pdf as an R expression.
// [[Rcpp::export]]
NumericVector c_d(int n, IntegerVector val, Rcpp::List const &params, int dtype) {
    Finitization* f = nullptr;
    NumericVector result(val.size());
    NumericVector par(params.size());
    switch(dtype) {
    case DistributionType::POISSON:
        if(params.containsElementNamed("theta")) {
            par[0] = Rcpp::as < double >( params["theta"]);
            f = new FinitizedPoissonDistribution(n, par[0]);
        }
        break;
    case DistributionType::LOGARITHMIC:
        if(params.containsElementNamed("theta")) {
            par[0] = Rcpp::as < double >( params["theta"]);
            f = new FinitizedLogarithmicDistribution(n, par[0]);
        }
        break;
    case DistributionType::BINOMIAL:
        if(params.containsElementNamed("N") && params.containsElementNamed("p")) {
            par[0] = Rcpp::as < int >( params["N"]);
            par[1] = Rcpp::as < double >( params["p"]);
            f = new FinitizedBinomialDistribution(n, par[1], par[0]);
        }
        else
            Rcerr << "Parameter N of the Binomial distribution not provided!" << endl;
        break;

    case DistributionType::NEGATIVEBINOMIAL:
        if(params.containsElementNamed("k") && params.containsElementNamed("q")) {
            par[0] = Rcpp::as < int >( params["k"]);
            par[1] = Rcpp::as < double >( params["q"]);
            f = new FinitizedNegativeBinomialDistribution(n, par[1], par[0]);
        }
        else
            Rcerr << "Parameter k of the Negative Binomial distribution not provided!" << endl;
        break;
    default:
        Rcerr << " Distribution type unsupported!" << endl;
    }

    if(f) {
        if(val.size() == 1)
            result[0] =  f->fin_pdf(val[0]);
        else {
            for(int i = 0; i< val.size(); ++i) {
                result[i] = f->fin_pdf(val[i]);
                //Rcout << result[i] << endl;
            }
        }
        delete f;
    }
    return result;
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

