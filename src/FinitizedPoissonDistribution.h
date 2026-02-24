/*
 * FinitizedPoissonDistribution.h
 *
 *  Created on: Jul 28, 2022
 *      Author: Bogdan.Oancea
 */

#ifndef FINITIZEDPOISSONDISTRIBUTION_H_
#define FINITIZEDPOISSONDISTRIBUTION_H_

#include "Finitization.h"
#include <ginac/ginac.h>

using namespace std;
using namespace GiNaC;

/**
 * @class FinitizedPoissonDistribution
 * @brief Concrete implementation of a finitized Poisson distribution.
 *
 * This class extends the abstract base class `Finitization` to provide a finitized version
 * of the Poisson distribution. It allows symbolic manipulation, evaluation, and sampling
 * from a distribution that preserves a specified number of moments from the original Poisson model.
 */
class FinitizedPoissonDistribution : public Finitization {
public:
    /**
     * @brief Constructor for finitized Poisson distribution.
     *
     * Initializes the internal structures for finitized Poisson distribution by setting
     * symbolic variables and computing finitized PDF values for support {0, ..., n}.
     *
     * @param n     The finitization order (i.e., number of moments to preserve).
     * @param theta The Poisson distribution rate parameter λ.
     */
    FinitizedPoissonDistribution(int n, double theta);

    /**
     * @brief Destructor.
     *
     * Performs clean-up; relies on base class for deeper memory management.
     */
    virtual ~FinitizedPoissonDistribution();

private:
    /**
     * @brief Symbolic form of the Poisson distribution's probability generating function.
     *
     * The Poisson generating function is given by \f$ \exp(\theta (x - 1)) \f$,
     * but for finitization we work with the formal series \f$ \exp(x) \f$.
     *
     * @param x     Symbol for the random variable.
     * @param theta Symbol for the Poisson rate parameter.
     * @return Symbolic expression representing the native PGF.
     */
    ex ntsd_base(symbol x, symbol theta) override;
};

#endif /* FINITIZEDPOISSONDISTRIBUTION_H_ */
