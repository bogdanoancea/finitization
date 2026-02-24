/*
 * FinitizedLogarithmicDistribution.h
 *
 *  Created on: Jul 28, 2022
 *      Author: Bogdan.Oancea
 */

#ifndef FINITIZEDLOGARITHMICDISTRIBUTION_H_
#define FINITIZEDLOGARITHMICDISTRIBUTION_H_

#include "Finitization.h"
#include <ginac/ginac.h>

using namespace std;
using namespace GiNaC;

/**
 * @class FinitizedLogarithmicDistribution
 * @brief Concrete implementation of a finitized Logarithmic distribution.
 *
 * This class inherits from the abstract base class `Finitization` and implements
 * the symbolic definition of the native Logarithmic distribution.
 * It supports symbolic expression generation and evaluation for finitized versions
 * of the Logarithmic distribution, preserving a user-defined number of moments.
 */
class FinitizedLogarithmicDistribution : public Finitization {
public:
    /**
     * @brief Constructor for the finitized Logarithmic distribution.
     *
     * Initializes the finitized form of the Logarithmic distribution by setting the
     * symbolic parameter and outcome variable, computing the finitized probability
     * mass function up to order `n`, and preparing the alias method structure for sampling.
     *
     * @param n     Finitization order (i.e., number of moments to preserve).
     * @param theta Logarithmic distribution parameter θ ∈ (0, 1).
     */
    FinitizedLogarithmicDistribution(int n, double theta);

    /**
     * @brief Destructor.
     *
     * Currently no additional cleanup is required. The base class handles most resources.
     */
    virtual ~FinitizedLogarithmicDistribution();

private:
    /**
     * @brief Native symbolic distribution form for the Logarithmic distribution.
     *
     * Returns the symbolic probability generating function (PGF) used to define the
     * logarithmic distribution. This form is tailored for symbolic manipulation in
     * GiNaC when constructing finitized distributions.
     *
     * The expression used is:
     * \f[
     *     \frac{\theta \log(1 - \theta - x)}{(\theta + x) \log(1 - \theta)}
     * \f]
     *
     * @param x     Symbol representing the outcome variable.
     * @param theta Symbol representing the parameter θ.
     * @return GiNaC symbolic expression of the PGF for the logarithmic distribution.
     */
    ex ntsd_base(symbol x, symbol theta) override;
};

#endif /* FINITIZEDLOGARITHMICDISTRIBUTION_H_ */
