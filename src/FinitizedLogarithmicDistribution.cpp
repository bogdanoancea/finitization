/*
 * FinitizedLogarithmicDistribution.cpp
 *
 *  Created on: Jul 28, 2022
 *      Author: Bogdan.Oancea
 */

#include "FinitizedLogarithmicDistribution.h"
#include <ginac/ginac.h>



using namespace std;

FinitizedLogarithmicDistribution::FinitizedLogarithmicDistribution(int n, double theta): Finitization(n) {
    m_theta = theta;                    // Store the parameter theta
    m_paramSymb = symbol("theta");     // Symbol for θ used in symbolic expressions
    m_x = symbol("x");                 // Symbol representing the outcome variable

    m_dprobs = new double[n + 1];      // Allocate space for finitized probability values

    // Compute finitized probabilities for all values from 0 to n
    for (int i = 0; i <= n; ++i) {
        m_dprobs[i] = fin_pdf(i);
    }

    // Initialize alias method structures for sampling
    setProbs(m_dprobs);

    // Mark initialization as complete
    m_finish = true;
}

FinitizedLogarithmicDistribution::~FinitizedLogarithmicDistribution() {
    // Destructor implementation (empty, no additional logic needed)
}

ex FinitizedLogarithmicDistribution::ntsd_base(symbol x, symbol theta){
    return theta * log(1 - theta -x) / ((theta + x) * log(1-theta));
}






