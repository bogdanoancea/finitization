#include <Rcpp.h>
#include <string>
#include "FinitizedLogarithmicDistribution.h"
#include "FinitizedPoissonDistribution.h"
#include "FinitizedBinomialDistribution.h"
#include "FinitizedNegativeBinomialDistribution.h"
#include "DistributionType.h"
#include <ginac/ginac.h>

using namespace std;
using namespace Rcpp;
using namespace GiNaC;

 //' Generate string representation of a finitized probability density function
 //'
 //' This function produces symbolic string representations of the probability
 //' density function (PMF) for a finitized distribution evaluated at specific values.
 //' The result can be either a plain-text R expression or a LaTeX-formatted string,
 //' depending on the \code{latex} parameter. Supported distributions include Poisson,
 //' Binomial, Negative Binomial, and Logarithmic.
 //'
 //' @param n An integer greater than 0 specifying the finitization order.
 //' @param val An integer vector of values at which to print the finitized PMF.
 //' @param params A named list of distribution-specific parameters:\cr
 //'   - Binomial: \code{list(N = trials)}\cr
 //'   - Negative Binomial: \code{list(k = dispersion)}
 //' @param dtype An integer code representing the distribution type.
 //'   Use helpers like \code{getPoissonType()}, \code{getBinomialType()}, etc.
 //' @param latex Logical; if \code{TRUE}, the output strings are formatted in LaTeX.
 //'   If \code{FALSE}, they are standard symbolic expressions.
 //'
 //' @return A \code{StringVector} containing the symbolic representation of the finitized PMF
 //'         for each element in \code{val}.
 //' @keywords internal
 //'
 //' @examples
 //' # Print finitized binomial PMF as symbolic expressions
 //' c_printDensity(n = 2, val = 0:2, params = list(N = 4), dtype = getBinomialType(), latex = FALSE)
 //'
 //' # Print LaTeX-formatted version
 //' c_printDensity(n = 2, val = 0:2, params = list(N = 4), dtype = getBinomialType(), latex = TRUE)
 //'
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

 //' Compute the probability mass function of a finitized distribution
 //'
 //' This function computes the finitized probability density (mass function)
 //' for a set of values, using the specified finitization order and distribution parameters.
 //' It supports the Poisson, Binomial, Negative Binomial, and Logarithmic distributions.
 //'
 //' @param n An integer greater than 0 specifying the finitization order.
 //' @param val An integer vector of values at which to evaluate the finitized probability density function.
 //' @param params A named list of distribution-specific parameters:\cr
 //'   - Poisson: \code{list(theta = value)}\cr
 //'   - Logarithmic: \code{list(theta = value)}\cr
 //'   - Binomial: \code{list(N = trials, p = probability)}\cr
 //'   - Negative Binomial: \code{list(k = dispersion, q = success probability)}
 //' @param dtype An integer code identifying the distribution type.
 //'   Use helpers like \code{getPoissonType()}, \code{getBinomialType()}, etc.
 //'
 //' @return A \code{NumericVector} of the same length as \code{val}, containing the evaluated finitized density values.
 //' @keywords internal
 //'
 //' @examples
 //' # Compute finitized binomial PMF values for 0, 1, ..., 4
 //' c_d(n = 3, val = 0:4, params = list(N = 4, p = 0.4), dtype = getBinomialType())
 //'
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


 //' Generate random values from a finitized distribution
 //'
 //' This function generates random variates from a finitized probability distribution,
 //' using the specified finitization order and distribution parameters.
 //' The supported distributions are: Poisson, Binomial, Negative Binomial, and Logarithmic.
 //'
 //' @param n An integer greater than 0 specifying the finitization order.
 //' @param params A named list of distribution-specific parameters:\cr
 //'   - Poisson: \code{list(theta = value)}\cr
 //'   - Logarithmic: \code{list(theta = value)}\cr
 //'   - Binomial: \code{list(N = trials, p = probability)}\cr
 //'   - Negative Binomial: \code{list(k = dispersion, q = success probability)}
 //' @param dtype An integer code specifying the distribution type.
 //'   Use helper functions like \code{getPoissonType()}, \code{getBinomialType()}, etc.
 //' @param no An integer specifying how many random values to generate.
 //'
 //' @return An \code{IntegerVector} of length \code{no} containing the generated random values.
 //' @keywords internal
 //'
 //' @examples
 //' # Generate 10 values from a finitized binomial distribution of order 3
 //' rvalues(n = 3, params = list(N = 10, p = 0.4), no = 10, dtype = getBinomialType())
 //'
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

 //' Compute the symbolic expression for \code{pdf(n - 1)} used in MFPS bounds
 //'
 //' This function generates the symbolic expression for the probability mass function (PMF)
 //' evaluated at \code{n - 1}, for a given finitization order and distribution type.
 //' It is used internally to determine the maximum feasible parameter space (MFPS) for finitized distributions.
 //'
 //' @param n An integer greater than 0 specifying the finitization order.
 //' @param params A named list of distribution-specific parameters. For example, use \code{list(N = 4)} for Binomial or \code{list(k = 3)} for Negative Binomial.
 //' @param dtype An integer code representing the distribution type (e.g., Poisson, Binomial, Negative Binomial, Logarithmic).
 //'
 //' @return A character string representing the symbolic expression of \code{pdf(n - 1)}.
 //' @keywords internal
 //'
 //' @examples
 //' # Internally used for MFPS calculation
 //' MFPS_pdf(n = 3, params = list(N = 5), dtype = getBinomialType())
 //'
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
            f = new FinitizedNegativeBinomialDistribution(n, 0.01, k);
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

 //' Return internal identifier for the Poisson distribution
 //'
 //' This helper function returns the internal integer constant used to
 //' identify the Poisson distribution within the finitization framework.
 //' It is primarily used for internal logic and function dispatching
 //' based on distribution type.
 //'
 //' @return An integer code representing the Poisson distribution.
 //' @keywords internal
 //'
 //' @examples
 //' # Used internally to specify distribution type
 //' getPoissonType()
 //'
 // [[Rcpp::export]]
 int getPoissonType() {
     return DistributionType::POISSON;
 }

 //' Return internal identifier for the Negative Binomial distribution
 //'
 //' This helper function returns the internal integer constant used to
 //' identify the Negative Binomial distribution within the finitization framework.
 //' It is primarily used for internal logic and function dispatching
 //' based on distribution type.
 //'
 //' @return An integer code representing the Negative Binomial distribution.
 //' @keywords internal
 //'
 //' @examples
 //' # Used internally to specify distribution type
 //' getNegativeBinomialType()
 //'
 // [[Rcpp::export]]
 int getNegativeBinomialType() {
     return DistributionType::NEGATIVEBINOMIAL;
 }


 //' Return internal identifier for the Binomial distribution
 //'
 //' This helper function returns the internal integer constant used to
 //' identify the Binomial distribution within the finitization framework.
 //' It is primarily used for internal logic and dispatching behavior
 //' based on distribution type.
 //'
 //' @return An integer code representing the Binomial distribution.
 //' @keywords internal
 //'
 //' @examples
 //' # Used internally to specify distribution type
 //' getBinomialType()
 //'
 // [[Rcpp::export]]
 int getBinomialType() {
     return DistributionType::BINOMIAL;
 }

 //' Return internal identifier for the Logarithmic distribution
 //'
 //' This helper function returns the internal integer constant used to
 //' identify the Logarithmic distribution within the finitization framework.
 //' It is primarily used for internal logic and dispatching behavior
 //' based on distribution type.
 //'
 //' @return An integer code representing the Binomial distribution.
 //' @keywords internal
 //'
 //' @examples
 //' # Used internally to specify distribution type
 //' getLogarithmicType()
 //'
 // [[Rcpp::export]]
 int getLogarithmicType() {
     return DistributionType::LOGARITHMIC;
 }



 //' Check symbolic equivalence of two expressions using GiNaC
 //'
 //' This function uses the GiNaC symbolic algebra system to determine whether
 //' two input expressions are mathematically equivalent. It parses the inputs,
 //' expands the difference, and checks if the result is symbolically zero.
 //'
 //' @param expr1_str A character string representing the first symbolic expression.
 //' @param expr2_str A character string representing the second symbolic expression.
 //'
 //' @return A logical value: `TRUE` if the expressions are symbolically equivalent,
 //'         `FALSE` otherwise (including in case of parsing errors).
 //' @examples
 //' check_symbolic_equivalence("q^2 + 2*q + 1", "(q + 1)^2")  # TRUE
 //' check_symbolic_equivalence("q^2", "q + 1")                # FALSE
 //'
 // [[Rcpp::export]]
bool check_symbolic_equivalence(std::string expr1_str, std::string expr2_str) {
    // Define symbol(s) used in expressions
    symbol q("q");

    // Use GiNaC parser to convert strings into symbolic expressions
    parser reader;
    try {
        ex expr1 = reader(expr1_str);
        ex expr2 = reader(expr2_str);

        // Compare expanded difference
        return expand(expr1 - expr2).is_zero();

    } catch (std::exception &e) {
        Rcpp::Rcout << "Error parsing expressions: " << e.what() << std::endl;
        return false;
    }
}


