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

FinitizedPoissonDistribution::FinitizedPoissonDistribution(int n, double theta): Finitization(n), m_theta(theta) {
	double probs[n+1];
	for( int i = 0; i <= n; ++i) {
		probs[i] = fin_pdf(i);
	}
	setProbs(probs);
}

FinitizedPoissonDistribution::~FinitizedPoissonDistribution() {
}

ex FinitizedPoissonDistribution::ntsd_base(symbol x, symbol theta){
	return exp(x);
}


string FinitizedPoissonDistribution::pdfToString(int val, bool tolatex) {
	stringstream result;
	symbol x("x");
	symbol param("theta");
	ex pdf_ = fin_pdfSymb(x, param, val);
	if(tolatex)
	    result << latex;
	result << pdf_;
    return result.str();
}


double FinitizedPoissonDistribution::fin_pdf(int val) {
	symbol x("x");
	symbol _param("theta");
	ex pdf_ = fin_pdfSymb(x, _param, val);
	return GiNaC::ex_to<GiNaC::numeric>(evalf(pdf_.subs(_param == m_theta))).to_double();
}





