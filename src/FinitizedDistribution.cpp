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
 * FinitizedDistribution.cpp
 *
 *  Created on: Jul 28, 2022
 *      Author: Bogdan.Oancea
 */

#include "FinitizedDistribution.h"
#include <queue>
#include <limits>
#include <utility>

using namespace std;

FinitizedDistribution::FinitizedDistribution(unsigned n): m_finitizationOrder(n), m_alias(n+1, {0.0, std::numeric_limits<size_t>::max()}), m_unif_int_distribution(0, n) {
	random_device device;
	m_generator.seed(device());
	for( unsigned i = 0; i <= n; i++) {
		m_values.push_back(i);
	}

}

FinitizedDistribution::~FinitizedDistribution() {
}

unsigned FinitizedDistribution::getFinitizationOrder() const {
	return m_finitizationOrder;
}

uniform_int_distribution<int> FinitizedDistribution::getUnifIntDistribution() const {
	return m_unif_int_distribution;
}

mt19937 FinitizedDistribution::getGenerator() const {
	return m_generator;
}

void FinitizedDistribution::generate_alias_table(const std::vector<double>& probs) {
    const size_t sz = probs.size();
    //std::vector<std::pair<double, size_t>> alias(sz, {0.0, std::numeric_limits<size_t>::max()});
    std::queue<size_t>  small, large;

    for (size_t i = 0; i != sz; ++i) {
      m_alias[i].first = sz * probs[i];
      if (m_alias[i].first < 1.0) {
        small.push(i);
      } else {
        large.push(i);
      }
    }

    while (not(small.empty()) and not(large.empty())) {
      auto s = small.front(), l = large.front();
      small.pop(), large.pop();
      m_alias[s].second = l;
      m_alias[l].first -= (1.0 - m_alias[s].first);

      if (m_alias[l].first < 1.0) {
        small.push(l);
      } else {
        large.push(l);
      }
    }
  }

vector<int> FinitizedDistribution::rvalues(unsigned no) {
	vector<int> result;
	size_t idx;
	for (unsigned i = 0; i < no; i++) {
		idx  = m_unif_int_distribution(m_generator);
		if (m_unif_double_distribution(m_generator) >= m_alias[idx].first and m_alias[idx].second != numeric_limits<size_t>::max()) {
			result.push_back(m_values[m_alias[idx].second]);
		} else {
			result.push_back(m_values[idx]);
		}

	}

	return result;
}
