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
//  * FinitizedLogarithmicDistribution.h
//  *
//  *  Created on: Jul 28, 2022
//  *      Author: Bogdan.Oancea
//  */
//

#ifndef FINITIZEDLOGARITHMICDISTRIBUTION_H_
#define FINITIZEDLOGARITHMICDISTRIBUTION_H_

#include "Finitization.h"
#include <ginac/ginac.h>

using namespace std;
using namespace GiNaC;


class FinitizedLogarithmicDistribution: public Finitization {
public:
    FinitizedLogarithmicDistribution(int n, double theta);
    virtual ~FinitizedLogarithmicDistribution();

    //double fin_pdf(int val) override;

private:
    ex ntsd_base(symbol x, symbol theta) override;

};

#endif /* FINITIZEDLOGARITHMICDISTRIBUTION_H_ */
