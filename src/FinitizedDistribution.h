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
 * FinitizedDistribution.h
 *
 *  Created on: Jul 28, 2022
 *      Author: Bogdan.Oancea
 */

#ifndef FINITIZEDDISTRIBUTION_H_
#define FINITIZEDDISTRIBUTION_H_

#include <vector>
#include <string>
#include <random>

using namespace std;

class FinitizedDistribution {
public:
	FinitizedDistribution(unsigned n);
	virtual ~FinitizedDistribution();

	virtual string pdfToString(unsigned val) = 0;
	virtual double pdf(unsigned val)= 0;
	vector<int> rvalues(unsigned no);

	unsigned getFinitizationOrder() const;
	mt19937 getGenerator() const;
	uniform_int_distribution<int> getUnifIntDistribution() const;

protected:
	void generate_alias_table(const std::vector<double>& probs);

private:
	unsigned m_finitizationOrder;
	uniform_real_distribution<double> m_unif_double_distribution;
	mt19937 m_generator;
	vector<std::pair<double, size_t>> m_alias;
	uniform_int_distribution<int> m_unif_int_distribution;
	vector<int> m_values;

};

#endif /* FINITIZEDDISTRIBUTION_H_ */
