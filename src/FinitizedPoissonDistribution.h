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
 * FinitizedPoissonDistribution.h
 *
 *  Created on: Jul 28, 2022
 *      Author: Bogdan.Oancea
 */

#ifndef FINITIZEDPOISSONDISTRIBUTION_H_
#define FINITIZEDPOISSONDISTRIBUTION_H_

#include "FinitizedDistribution.h"
#include <ginac/ginac.h>

using namespace std;
using namespace GiNaC;


class FinitizedPoissonDistribution: public FinitizedDistribution {
public:
	FinitizedPoissonDistribution(unsigned n, double theta);
	virtual ~FinitizedPoissonDistribution();

	string pdfToString(unsigned val) override;
	double pdf(unsigned val) override;
	//vector<double> rvalues(unsigned no) override;

private:
	double m_theta;
	ex poisson_ntsd_base(symbol x);
	ex poisson_ntsf(symbol x, ex pnb, int n);
	ex fin_pdf_poisson(symbol x, symbol _theta, ex ntsf,  unsigned int x_val);
	ex poisson_pdf(symbol x, symbol param, int n, int x_val);

};

#endif /* FINITIZEDPOISSONDISTRIBUTION_H_ */
