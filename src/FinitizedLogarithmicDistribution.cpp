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
//  * FinitizedLogarithmicDistribution.cpp
//  *
//  *  Created on: Jul 28, 2022
//  *      Author: Bogdan.Oancea
//  */
//
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






