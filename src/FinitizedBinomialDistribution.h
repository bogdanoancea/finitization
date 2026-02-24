/*
 * FinitizedBinomialDistribution.h
 *
 *  Created on: Jul 28, 2022
 *      Author: Bogdan Oancea
 */

#ifndef FINITIZEDBINOMIALDISTRIBUTION_H_
#define FINITIZEDBINOMIALDISTRIBUTION_H_

#include "Finitization.h"
#include <ginac/ginac.h>

using namespace std;
using namespace GiNaC;

/**
 * @class FinitizedBinomialDistribution
 * @brief Concrete implementation of finitized Binomial distribution.
 *
 * This class inherits from the abstract base class `Finitization` and implements
 * the required symbolic definition of the native Binomial probability mass function.
 * It supports symbolic expression construction and random sampling from a finitized
 * version of the Binomial distribution, preserving a specified number of moments.
 */
class FinitizedBinomialDistribution : public Finitization {
public:
    /**
     * @brief Constructor for finitized Binomial distribution.
     *
     * Initializes internal symbolic and numeric structures for computing finitized
     * probability mass function values up to the specified order `n`. Sets the
     * symbolic parameter name as "p" and the random variable as "x". Also prepares
     * the internal probability vector for efficient sampling.
     *
     * @param n Finitization order (number of moments to preserve).
     * @param p Probability of success in a single trial.
     * @param N Number of trials in the binomial experiment.
     */
    FinitizedBinomialDistribution(int n, double theta, int N);

    /**
    * @brief Destructor.
    *
    * Clean-up is handled via base class and owned memory is deleted safely.
    */
    virtual ~FinitizedBinomialDistribution();

private:
    int m_N;  ///< Number of trials in the Binomial distribution

    /**
     * @brief Symbolic form of the native Binomial distribution's probability generating function.
     *
     * This function returns the symbolic form of the moment-generating expression:
     * \f$ (1 + x)^N \f$ which corresponds to the Binomial generating function.
     *
     * @param x Symbol representing the outcome.
     * @param theta Symbolic probability of success (not used in the PGF here, but retained for compatibility).
     * @return GiNaC symbolic expression representing \f$ (1 + x)^N \f$.
     */
    ex ntsd_base(symbol x, symbol theta) override;
};

#endif /* FINITIZEDBINOMIALDISTRIBUTION_H_ */
