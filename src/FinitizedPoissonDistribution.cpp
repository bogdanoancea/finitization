/*
 * Copyright (C) 2019  Bogdan Oancea

 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version and under the EUPL free software license version 1.0 or later.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/> and
 * <https://ec.europa.eu/info/european-union-public-licence_en>
 *
 * FinitizedPoissonDistribution.cpp
 *
 *  Created on: Jul 28, 2022
 *      Author: Bogdan.Oancea
 */

#include "FinitizedPoissonDistribution.h"
#include <iostream>
#include <ginac/ginac.h>
#include <sstream>
#include <string>
#include <cassert>
#include <cmath>
#include <iostream>
#include <random>
#include <algorithm>
#include <limits>
#include <functional>
#include <map>
#include <vector>
#include <queue>


using namespace std;

FinitizedPoissonDistribution::FinitizedPoissonDistribution(unsigned n, double theta): FinitizedDistribution(n), m_theta(theta) {
	vector<double> probs;
	for( unsigned i = 0; i <= n; i++) {
		probs.push_back(pdf(i));
		//cout << probs[i] << endl;
	}
	generate_alias_table(probs);
}

FinitizedPoissonDistribution::~FinitizedPoissonDistribution() {
}

ex FinitizedPoissonDistribution::poisson_ntsd_base(symbol x){
	return exp(x);
}

ex FinitizedPoissonDistribution::poisson_ntsf(symbol x, ex pnb, int n) {
	return series_to_poly(pnb.series(x == 0, n+1));

}

ex FinitizedPoissonDistribution::fin_pdf_poisson(symbol x, symbol _theta, ex ntsf, unsigned int x_val) {
	ex optheta = -_theta;
	ex pdf = (ntsf.diff(x, x_val));
	pdf = pdf.subs(x == optheta) * pow(_theta, x_val) / factorial(x_val);
	return pdf;

}

ex FinitizedPoissonDistribution::poisson_pdf(symbol x, symbol param, int n, int x_val) {
	ex pdf = fin_pdf_poisson(x, param, poisson_ntsf(x, poisson_ntsd_base(x),n), x_val);
	return pdf;
}

string FinitizedPoissonDistribution::pdfToString(unsigned val) {
	stringstream result;
	symbol x("x");
	symbol param("theta");
	ex pdf = poisson_pdf(x, param, getFinitizationOrder(), val);
	result << pdf;
	return result.str();
}


double FinitizedPoissonDistribution::pdf(unsigned val) {
	stringstream result;
	symbol x("x");
	symbol _param("theta");
	ex pdf = poisson_pdf(x, _param, getFinitizationOrder(), val);
	result << pdf.subs(_param == m_theta);
	return std::stod(result.str());

}




