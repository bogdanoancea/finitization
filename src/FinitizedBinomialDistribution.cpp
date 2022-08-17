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
#include "FinitizedBinomialDistribution.h"
#include <ginac/ginac.h>

using namespace std;

FinitizedBinomialDistribution::FinitizedBinomialDistribution(int n, double p, int N): Finitization(n) , m_N(N) {
    m_theta = p;
    m_paramSymb = symbol("p");
    m_x = symbol("x");
    m_dprobs = new double[n+1];
    for( int i = 0; i <= n; ++i) {
        m_dprobs[i] = fin_pdf(i);
    }
    setProbs(m_dprobs);
    m_finish = true;
}

FinitizedBinomialDistribution::~FinitizedBinomialDistribution() {
}

ex FinitizedBinomialDistribution::ntsd_base(symbol x, symbol theta){
    return pow((1 + x), m_N);
}

