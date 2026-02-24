/*
 * FinitizedNegativeBinomialDistribution.h
 *
 *  Created on: Jul 28, 2022
 *      Author: Bogdan.Oancea
 */

#ifndef FINITIZEDNEGATIVEBINOMIALDISTRIBUTION_H_
#define FINITIZEDNEGATIVEBINOMIALDISTRIBUTION_H_

#include "Finitization.h"
#include <ginac/ginac.h>

using namespace std;
using namespace GiNaC;

/**
 * @class FinitizedNegativeBinomialDistribution
 * @brief Concrete implementation of a finitized Negative Binomial distribution.
 *
 * This class extends the abstract base class `Finitization` to provide a finitized version
 * of the Negative Binomial distribution. It implements the symbolic native probability
 * mass function, which can then be transformed into a finitized version preserving
 * a specified number of moments.
 */
class FinitizedNegativeBinomialDistribution : public Finitization {
public:
    /**
     * @brief Constructor for finitized Negative Binomial distribution.
     *
     * Initializes symbolic variables and precomputes finitized probability values
     * for all valid support values {0, ..., n}. Uses the symbolic parameter "q"
     * (defined as q = 1 - p) and stores the number of required failures k.
     *
     * @param n The finitization order (number of moments to preserve).
     * @param p The success probability (used to define q = 1 - p).
     * @param k The dispersion parameter (number of failures) in the Negative Binomial distribution.
     */
    FinitizedNegativeBinomialDistribution(int n, double p, int k);

    /**
     * @brief Destructor for clean-up.
     *
     * Resources are released via the base class; nothing specific needed here.
     */
    virtual ~FinitizedNegativeBinomialDistribution();

private:
    int m_k;  ///< Number of failures parameter for the Negative Binomial distribution

    /**
     * @brief Native symbolic distribution definition for Negative Binomial.
     *
     * Constructs the symbolic expression for the Negative Binomial distribution's
     * probability generating function (PGF), based on the known form:
     * \f[
     *     \left( \frac{1}{1 - \frac{x}{1 - q}} \right)^k
     * \f]
     * where q = 1 - p.
     *
     * @param x Symbol representing the random variable.
     * @param theta Symbol representing q = 1 - p.
     * @return A symbolic GiNaC expression for the PGF of the Negative Binomial distribution.
     */
    ex ntsd_base(symbol x, symbol theta) override;
};

#endif /* FINITIZEDNEGATIVEBINOMIALDISTRIBUTION_H_ */
