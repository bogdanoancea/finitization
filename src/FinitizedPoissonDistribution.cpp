/*
 * FinitizedPoissonDistribution.cpp
 *
 *  Created on: Jul 28, 2022
 *      Author: Bogdan.Oancea
 */
#include "FinitizedPoissonDistribution.h"
#include <ginac/ginac.h>

using namespace std;

FinitizedPoissonDistribution::FinitizedPoissonDistribution(int n, double theta): Finitization(n) {
	m_theta = theta;                      // Store the lambda parameter
	m_paramSymb = symbol("theta");       // Symbol used in symbolic expressions for lambda
	m_x = symbol("x");                   // Symbol representing the discrete outcome variable

	m_dprobs = new double[n + 1];        // Allocate memory for finitized probabilities

	// Compute finitized PDF for all values from 0 to n
	for (int i = 0; i <= n; ++i) {
	    m_dprobs[i] = fin_pdf(i);
	}

	// Initialize internal sampling structure with computed probabilities
	setProbs(m_dprobs);

	// Mark setup as completed
	m_finish = true;
}

FinitizedPoissonDistribution::~FinitizedPoissonDistribution() {
    // Nothing to explicitly clean here — handled by base class and destructors.
}

ex FinitizedPoissonDistribution::ntsd_base(symbol x, symbol theta){
	return exp(x);
}






