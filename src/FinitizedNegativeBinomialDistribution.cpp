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
//  * FinitizedBinomialDistribution.cpp
//  *
//  *  Created on: Jul 28, 2022
//  *      Author: Bogdan.Oancea
//  */
//
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

