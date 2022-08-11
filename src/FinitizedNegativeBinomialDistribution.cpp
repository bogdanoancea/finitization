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
    m_theta = p;
    m_paramSymb = symbol("q");
    m_x = symbol("x");
    double probs[n+1];
    for( int i = 0; i <= n; ++i) {
        probs[i] = fin_pdf(i);
    }
    setProbs(probs);
}

FinitizedNegativeBinomialDistribution::~FinitizedNegativeBinomialDistribution() {
}

ex FinitizedNegativeBinomialDistribution::ntsd_base(symbol x, symbol theta){
    return pow( 1 /(1-x/(1-theta)), m_k);
}


double FinitizedNegativeBinomialDistribution::fin_pdf(int val) {
    ex pdf_ = fin_pdfSymb(m_x, m_paramSymb, val);
    return GiNaC::ex_to<GiNaC::numeric>(evalf( pdf_.subs(m_paramSymb == m_theta))).to_double();
}
