/*
 * FinitizedBinomialDistribution.cpp
 *
 *  Created on: Jul 28, 2022
 *      Author: Bogdan Oancea
 */
#include "FinitizedBinomialDistribution.h"
#include <ginac/ginac.h>

using namespace std;

FinitizedBinomialDistribution::FinitizedBinomialDistribution(int n, double p, int N): Finitization(n) , m_N(N) {
    m_theta = p;                          // Set the success probability
    m_paramSymb = symbol("p");           // Symbol for the probability parameter
    m_x = symbol("x");                   // Symbol for the outcome variable
    m_dprobs = new double[n + 1];        // Allocate memory for finitized probabilities

    // Compute finitized PDF values for x = 0 to n and store in m_dprobs
    for (int i = 0; i <= n; ++i) {
        m_dprobs[i] = fin_pdf(i);
    }

    // Initialize the internal alias sampling table
    setProbs(m_dprobs);

    // Mark that initialization has finished
    m_finish = true;
}

FinitizedBinomialDistribution::~FinitizedBinomialDistribution() {
    // No explicit cleanup needed in this implementation
}

ex FinitizedBinomialDistribution::ntsd_base(symbol x, symbol theta){
    return pow((1 + x), m_N);
}

