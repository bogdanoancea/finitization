#include <random>
#include "Rcpp.h"
#include <ginac/ginac.h>

using namespace std;
using namespace Rcpp;
using namespace GiNaC;

#ifndef FINITIZATION_H_
#define FINITIZATION_H_

/**
 * @class Finitization
 * @brief Abstract base class for finitized probability distributions.
 *
 * This class provides the core symbolic and numerical functionality required
 * to implement finitized versions of discrete probability distributions.
 * Derived classes must implement the symbolic definition of the native
 * distribution function via the pure virtual method `ntsd_base()`.
 */
class Finitization {

public:
    /**
     * @brief Constructor for the Finitization base class.
     *
     * Initializes the sampling structures and RNG setup. This class provides
     * the foundation for finitized distributions and supports symbolic and
     * numeric operations, such as PDF evaluation and random variate generation.
     *
     * @param n The finitization order (i.e., number of preserved moments).
     */
    Finitization(int n);

   /** @brief Destructor.
    *
    * Releases dynamically allocated resources used for alias method and probability storage.
    */
    virtual ~Finitization();

    /**
     * @brief Returns the symbolic or LaTeX representation of the PDF.
     *
     * @param val Value of the random variable.
     * @param tolatex Whether to use LaTeX output format.
     * @return A string representation of the symbolic PDF.
     */
    string pdfToString(int val, bool tolatex = false);

    /**
     * @brief Generates random samples using the alias method.
     *
     * @param no Number of values to generate.
     * @return An IntegerVector containing the sampled values.
     */
    IntegerVector rvalues(int no);

    /**
     * @brief Computes the numeric value of the finitized PDF.
     *
     * @param val Value of the variable to evaluate.
     * @return A double representing the finitized PDF value.
     */
    double fin_pdf(int val);

protected:
    /**
     * @brief Initializes alias method tables from a probability vector.
     *
     * Transforms the input probability vector into alias and probability arrays
     * for efficient sampling using the alias method.
     *
     * @param p Pointer to the probability vector.
     */
    void setProbs(double *probs);

    /**
     * @brief Computes the finitized form of the generating function.
     *
     * Uses GiNaC’s symbolic series expansion and caches the result.
     *
     * @param pnb The symbolic native transform series (PGF).
     * @return Symbolic polynomial of the series truncated at finitization order.
     */
    ex ntsf(ex pnb);

    /**
     * @brief Computes the symbolic PDF at a specific value.
     *
     * Computes the symbolic PDF using the nth derivative of the finitized PGF.
     * Includes memoization to cache intermediate derivatives.
     *
     * @param ntsf Symbolic finitized PGF.
     * @param x_val Value at which to compute the PDF.
     * @return Symbolic expression for PDF at x_val.
     */
    ex pdf(ex ntsf, int x_val);

    /**
     * @brief Computes symbolic PDF for a value.
     *
     * Wrapper that evaluates the full symbolic PDF for the given value.
     *
     * @param x_val Integer value of the random variable.
     * @return Symbolic expression of the PDF.
     */
    ex fin_pdfSymb(int x_val);

    /**
     * @brief Abstract method to provide the symbolic base PDF for a given distribution.
     * Must be implemented in derived classes.
     * @param x Symbol representing the random variable.
     * @param theta Distribution parameter (symbolic).
     * @return Symbolic native PDF (not finitized).
     */
    virtual ex ntsd_base(symbol x, symbol theta) = 0;

    int m_finitizationOrder;        ///< Order of the finitization (number of moments preserved)
    double m_theta;                 ///< Parameter value used in the distribution
    symbol m_paramSymb;            ///< Symbol representing the distribution parameter (e.g., p, theta)
    symbol m_x;                    ///< Symbol representing the random variable x
    double* m_dprobs;              ///< Pointer to numerical probabilities for sampling
    bool m_finish;                 ///< Used for internal state tracking

private:
    std::uniform_real_distribution<double> m_unif_double_distribution; ///< Uniform real generator
    std::uniform_int_distribution<int> m_unif_int_distribution;        ///< Uniform integer generator
    std::mt19937 m_generator;                                          ///< Mersenne Twister engine for randomness

    int* m_alias;               ///< Alias table for sampling
    double* m_prob;             ///< Probability table for sampling
    int* m_values;              ///< Support values associated with the distribution

    bool m_ntsfFirstTime;       ///< Used to delay computation of ntsf form
    ex m_ntsfSymb;              ///< Cached symbolic form of the normalized truncated series
    std::unordered_map<int, ex> m_cache; ///< Cache of symbolic evaluations at specific values
};

#endif /* FINITIZATION_H_ */
