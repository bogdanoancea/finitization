/*
 * FinitizedNegativeBinomialDistribution.cpp
 *
 *  Created on: Jul 28, 2022
 *      Author: Bogdan.Oancea
 */
#include "FinitizedNegativeBinomialDistribution.h"
#include <ginac/ginac.h>

using namespace std;


FinitizedNegativeBinomialDistribution::FinitizedNegativeBinomialDistribution(int n, double p, int k): Finitization(n),  m_k(k) {
    m_theta = p;                         // Store p; internally we work with q = 1 - p
    m_paramSymb = symbol("q");          // Symbol for the transformed parameter q
    m_x = symbol("x");                  // Symbol for the random variable

    m_dprobs = new double[n + 1];       // Allocate memory for finitized probabilities

    // Compute and cache PDF values for the finitized distribution
    for (int i = 0; i <= n; ++i) {
        m_dprobs[i] = fin_pdf(i);
    }

    // Initialize the alias sampling structure
    setProbs(m_dprobs);

    // Mark the setup as finished
    m_finish = true;
}

FinitizedNegativeBinomialDistribution::~FinitizedNegativeBinomialDistribution() {
}

ex FinitizedNegativeBinomialDistribution::ntsd_base(symbol x, symbol theta){
    return pow( 1 /(1-x/(1-theta)), m_k);
}

