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
#include <iostream>
#include <ginac/ginac.h>
#include <sstream>
#include <string>
#include <random>
#include <limits>


using namespace std;

FinitizedLogarithmicDistribution::FinitizedLogarithmicDistribution(int n, double theta): Finitization(n), m_theta(theta) {
    double probs[n+1];
    for( int i = 0; i <= n; ++i) {
        probs[i] = fin_pdf(i);
    }
    setProbs(probs);
}

FinitizedLogarithmicDistribution::~FinitizedLogarithmicDistribution() {
}

ex FinitizedLogarithmicDistribution::ntsd_base(symbol x, symbol theta){
    return theta * log(1 - theta -x) / ((theta + x) * log(1-theta));
}

ex FinitizedLogarithmicDistribution::ntsf(symbol x, ex pnb) {
    return series_to_poly(pnb.series(x == 0, m_finitizationOrder+1));

}

ex FinitizedLogarithmicDistribution::pdf(symbol x, symbol _theta, ex ntsf, int x_val) {
    ex optheta = -_theta;
    ex pdf = (ntsf.diff(x, x_val));
    pdf = pdf.subs(x == optheta) * pow(_theta, x_val) / factorial(x_val);
    return pdf;

}

ex FinitizedLogarithmicDistribution::fin_pdf(symbol x, symbol param, int x_val) {
    ex pdf_ = pdf(x, param, ntsf(x, ntsd_base(x, param)), x_val);
    return pdf_;
}

string FinitizedLogarithmicDistribution::pdfToString(int val, bool tolatex) {
    stringstream result;
    symbol x("x");
    symbol param("theta");
    ex pdf_ = fin_pdf(x, param, val);
    if(tolatex)
        result << latex;
    result << pdf_;
    return result.str();
}

double FinitizedLogarithmicDistribution::getProb(int val)  {
    return fin_pdf(val);
}

double FinitizedLogarithmicDistribution::fin_pdf(int val) {
    stringstream result;
    symbol x("x");
    symbol _param("theta");
    ex pdf_ = fin_pdf(x, _param, val);
    result << evalf(pdf_.subs(_param == m_theta));
    return std::stod(result.str());
}



double FinitizedLogarithmicDistribution::fin_pdf(int val, double theta) {
    stringstream result;
    symbol x("x");
    symbol _param("theta");
    ex pdf_ = fin_pdf(x, _param, val);
    result << evalf(pdf_.subs(_param == theta));
    return std::stod(result.str());
}




