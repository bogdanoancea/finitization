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

FinitizedBinomialDistribution::FinitizedBinomialDistribution(int n, double p, int N): Finitization(n), m_theta(p), m_N(N) {
    double probs[n+1];
    for( int i = 0; i <= n; ++i) {
        probs[i] = fin_pdf(i);
    }
    setProbs(probs);
}

FinitizedBinomialDistribution::~FinitizedBinomialDistribution() {
}

ex FinitizedBinomialDistribution::ntsd_base(symbol x, symbol theta){
    return pow((1 + x), m_N);
}


ex FinitizedBinomialDistribution::pdf(symbol x, symbol _theta, ex ntsf, int x_val) {
    ex optheta = -_theta;
    ex pdf = (ntsf.diff(x, x_val));
    pdf = pdf.subs(x == optheta) * pow(_theta, x_val) / factorial(x_val);
    return pdf;

}
//candidat la mers in clasa de baza
ex FinitizedBinomialDistribution::fin_pdf(symbol x, symbol param, int x_val) {
    ex pdf_ = pdf(x, param, ntsf(x, ntsd_base(x, param)), x_val);
    return pdf_;
}

string FinitizedBinomialDistribution::pdfToString(int val, bool tolatex) {
    stringstream result;
    symbol x("x");
    symbol param("p");
    //symbol N("N");
    ex pdf_ = fin_pdf(x, param, val);
    if(tolatex)
        result << latex;
    result << pdf_;
    return result.str();
}

//in clasa de baza
double FinitizedBinomialDistribution::getProb(int val)  {
    return fin_pdf(val);
}

double FinitizedBinomialDistribution::fin_pdf(int val) {
    symbol x("x");
    symbol _param("p");
    //symbol N("N");
    ex pdf_ = fin_pdf(x, _param, val);
    return GiNaC::ex_to<GiNaC::numeric>(evalf(pdf_.subs(_param == m_theta))).to_double();
}
