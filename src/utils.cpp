#include <Rcpp.h>
#include <string>
#include "FinitizedLogarithmicDistribution.h"
#include "FinitizedPoissonDistribution.h"
#include "FinitizedBinomialDistribution.h"
#include "FinitizedNegativeBinomialDistribution.h"
#include "DistributionType.h"


using namespace std;

//' Generates a string representation of the probability density function.
//'
//' @param n The finitization order. It should be an integer > 0.
//' @param val The value of the variable for which the probability density function is computed.
//' @param params Other parameters of the distribution. They are provided in a list with named items, where the name
//' of an item is the name of the parameter.
//' @param dtype The type of the distribution: Poisson, Binomial, NegativeBinomial, Logaritmic.
//' @param latex If true it returns a Latex formatted string representation of the pdf, otherwise it returns
//' the string representation of the pdf as an R expression.
//' @keywords internal
//' @return a string representation of the pdf.
// [[Rcpp::export]]
StringVector c_printDensity(int n, IntegerVector val, Rcpp::List const &params, int dtype, bool latex = false) {

    Finitization* f = nullptr;
    StringVector result(val.size());
    double N, k;

    switch(dtype) {
    case DistributionType::POISSON:
        f = new FinitizedPoissonDistribution(n, 0.1);
        break;
    case DistributionType::LOGARITHMIC:
        f = new FinitizedLogarithmicDistribution(n, 0.01);
        break;
    case DistributionType::BINOMIAL:
        if(params.containsElementNamed("N")) {
            N = Rcpp::as < int >( params["N"]);
            f = new FinitizedBinomialDistribution(n, 0, N);
        }
        else
            Rcerr << "Parameter N of the Binomial distribution not provided!" << endl;
        break;

    case DistributionType::NEGATIVEBINOMIAL:
        if(params.containsElementNamed("k")) {
            k = Rcpp::as < int >( params["k"]);
            f = new FinitizedNegativeBinomialDistribution(n, 0, k);
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
        }
        delete f;
    }
    return result;

}

//' Computes the probability density of a finitized distribution.
//'
//' @param n The finitization order. It should be an integer > 0.
//' @param val The value of the variable for which the probability density function is computed.
//' @param params Other parameters of the distribution. They are provided in a list with named items, where the name
//' of an item is the name of the parameter.
//' @param dtype The type of the distribution: Poisson, Binomial, NegativeBinomial, Logaritmic.
//' @keywords internal
//' @return a\code{NumericVector} with the values of the density for each value provide in \code{val}.
// [[Rcpp::export]]
NumericVector c_d(int n, IntegerVector val, Rcpp::List const &params, int dtype) {
    Finitization* f = nullptr;
    NumericVector result(val.size());
    double theta, N, p, k, q;
    switch(dtype) {
    case DistributionType::POISSON:
        if(params.containsElementNamed("theta")) {
            theta = Rcpp::as < double >( params["theta"]);
            f = new FinitizedPoissonDistribution(n, theta);
        }
        break;
    case DistributionType::LOGARITHMIC:
        if(params.containsElementNamed("theta")) {
            theta = Rcpp::as < double >( params["theta"]);
            f = new FinitizedLogarithmicDistribution(n, theta);
        }
        break;
    case DistributionType::BINOMIAL:
        if(params.containsElementNamed("N") && params.containsElementNamed("p")) {
            N = Rcpp::as < int >( params["N"]);
            p = Rcpp::as < double >( params["p"]);
            f = new FinitizedBinomialDistribution(n, p, N);
        }
        else
            Rcerr << "Binomial distribution parameter(s) not provided!" << endl;
        break;
    case DistributionType::NEGATIVEBINOMIAL:
        if(params.containsElementNamed("k") && params.containsElementNamed("q")) {
            k = Rcpp::as < int >( params["k"]);
            q = Rcpp::as < double >( params["q"]);
            f = new FinitizedNegativeBinomialDistribution(n, q, k);
        }
        else
            Rcerr << "Negative Binomial distribution parameter(s) not provided!" << endl;
        break;
    default:
        Rcerr << " Distribution type unsupported!" << endl;
    }

    if(f) {
        for(int i = 0; i< val.size(); ++i)
            result[i] = f->fin_pdf(val[i]);
        delete f;
    }

    return result;
}


//' Random values generation.
//'
//' @param n The finitization order. It should be an integer > 0.
//' @param params Other parameters of the distribution. They are provided in a list with named items, where the name
//' of an item is the name of the parameter.
//' @param dtype The type of the distribution: Poisson, Binomial, NegativeBinomial, Logaritmic.
//' @param no The number of random values to be generated.
//' @keywords internal
//' @return a\code{NumericVector} with the random values generated.
// [[Rcpp::export]]
IntegerVector rvalues(int n, Rcpp::List const &params, int no, int dtype) {
    Finitization* f = nullptr;
    IntegerVector result(no);
    double theta, N, p, k, q;
    switch(dtype) {
    case DistributionType::POISSON:
        if(params.containsElementNamed("theta")) {
            theta = Rcpp::as < double >( params["theta"]);
            f = new FinitizedPoissonDistribution(n, theta);
        }
        break;
    case DistributionType::LOGARITHMIC:
        if(params.containsElementNamed("theta")) {
            theta = Rcpp::as < double >( params["theta"]);
            f = new FinitizedLogarithmicDistribution(n, theta);
        }
        break;
    case DistributionType::BINOMIAL:
        if(params.containsElementNamed("N") && params.containsElementNamed("p")) {
            N = Rcpp::as < int >( params["N"]);
            p = Rcpp::as < double >( params["p"]);
            f = new FinitizedBinomialDistribution(n, p, N);
        }
        else
            Rcerr << "Binomial distribution parameter(s) not provided!" << endl;
        break;
    case DistributionType::NEGATIVEBINOMIAL:
        if(params.containsElementNamed("k") && params.containsElementNamed("q")) {
            k = Rcpp::as < int >( params["k"]);
            q = Rcpp::as < double >( params["q"]);
            f = new FinitizedNegativeBinomialDistribution(n, q, k);
        }
        else
            Rcerr << "Negative Binomial distribution parameter(s) not provided!" << endl;
        break;
    default:
        Rcerr << " Distribution type unsupported!" << endl;
    }

    if(f) {
        result = f->rvalues(no);
        delete f;
    }
    return result;
}

//' Computes the expression of the \code{pdf(n-1)} needed to compute the maximu feasible parameter space.
//'
//' @param n The finitization order. It should be an integer > 0.
//' @param params Other parameters of the distribution. They are provided in a list with named items, where the name
//' of an item is the name of the parameter.
//' @param dtype The type of the distribution: Poisson, Binomial, NegativeBinomial, Logaritmic.
//' @keywords internal
//' @return the expression of the \code{pdf(n-1)}.
// [[Rcpp::export]]
String MFPS_pdf(int n, Rcpp::List const &params, int dtype ) {
    Finitization* f = nullptr;
    String result;
    double  N, k;
    switch(dtype) {
    case DistributionType::POISSON:
        f = new FinitizedPoissonDistribution(n, 0.01);
        break;
    case DistributionType::LOGARITHMIC:
        f = new FinitizedLogarithmicDistribution(n, 0.01);
        break;
    case DistributionType::BINOMIAL:
        if(params.containsElementNamed("N") ) {
            N = Rcpp::as < int >( params["N"]);
            f = new FinitizedBinomialDistribution(n, 0.1, N);
        }
        else
            Rcerr << "Binomial distribution parameter(s) not provided!" << endl;
        break;
    case DistributionType::NEGATIVEBINOMIAL:
        if(params.containsElementNamed("k")) {
            k = Rcpp::as < int >( params["k"]);
            f = new FinitizedNegativeBinomialDistribution(n, 0.1, k);
        }
        else
            Rcerr << "Negative Binomial distribution parameter(s) not provided!" << endl;
        break;
    default:
        Rcerr << " Distribution type unsupported!" << endl;
    }

    if(f) {
        result = f->pdfToString(n-1);
        delete f;
    }
    return result;

}

//' The Poisson distribution type.
//'
//' @return the Poisson distribution type.
//' @keywords internal
// [[Rcpp::export]]
int getPoissonType() {
    return DistributionType::POISSON;
}

//' The Negative Binomial distribution type.
//'
//' @return the Negative Binomial distribution type.
//' @keywords internal
// [[Rcpp::export]]
int getNegativeBinomialType() {
    return DistributionType::NEGATIVEBINOMIAL;
}


//' The Binomial distribution type.
//'
//' @return the Binomial distribution type.
//' @keywords internal
// [[Rcpp::export]]
int getBinomialType() {
    return DistributionType::BINOMIAL;
}


//' The Logarithmic distribution type.
//'
//' @return the Logarithmic distribution type.
//' @keywords internal
// [[Rcpp::export]]
int getLogarithmicType() {
    return DistributionType::LOGARITHMIC;
}




