// /*
//  * Copyright (C) 2019  Bogdan Oancea
//
//  * This program is free software: you can redistribute it and/or modify
//  * it under the terms of the GNU General Public License as published by
//  * the Free Software Foundation, either version 3 of the License, or
//  * any later version and under the EUPL free software license version 1.0 or later.
//  *
//  * This program is distributed in the hope that it will be useful,
//  * but WITHOUT ANY WARRANTY; without even the implied warranty of
//  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  * GNU General Public License for more details.
//  * You should have received a copy of the GNU General Public License
//  * along with this program.  If not, see <http://www.gnu.org/licenses/> and
//  * <https://ec.europa.eu/info/european-union-public-licence_en>
//  *
//  * FinitizedPoissonDistribution.cpp
//  *
//  *  Created on: Jul 28, 2022
//  *      Author: Bogdan.Oancea
//  */
//
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






