#include "Finitization.h"
#include <iostream>
#include <Rcpp.h>
#include <chrono>

using namespace std;
using namespace std::chrono;

Finitization::Finitization(int n): m_finitizationOrder(n), m_unif_double_distribution{0.0, 1.0}, m_unif_int_distribution(0,n) {
     m_alias = new int[m_finitizationOrder+1];
     m_prob = new double[m_finitizationOrder+1];
     m_values = new int[m_finitizationOrder+1];
     for(int i = 0; i <= n; i++)
        m_values[i] = i;
	 m_generator.seed(time(0));

}

Finitization::~Finitization() {
    delete[] m_alias;
    delete[] m_prob;
    delete[] m_values;
}

void Finitization::setProbs(double* p) {
    int i, a, g;
    int tmp = m_finitizationOrder + 1;
    int *S = new int[tmp];
    int *L = new int[tmp];
    double *P = new double[tmp];

    double sum=0.0;
    for (i = 0; i < tmp; ++i ) {
        sum += p[i];
    }
    for (i = 0; i < tmp; ++i )
        P[i] = p[i] * (tmp) / sum;

    int nS = 0, nL = 0;
    for (i = m_finitizationOrder; i>=0; --i ) {
        P[i] < 1 ? (S[nS++] = i) : (L[nL++] = i);
    }
    while ( nS && nL ) {
        a = S[--nS];
        g = L[--nL];
        m_prob[a] = P[a];
        m_alias[a] = g;
        P[g] += P[a] - 1;
        P[g] < 1 ? (S[nS++] = g) : (L[nL++] = g) ;
    }

    while ( nL )
        m_prob[ L[--nL] ] = 1;

    while ( nS )
        m_prob[ S[--nS] ] = 1;

    delete[] S;
    delete[] L;
    delete[] P;

}


IntegerVector Finitization::rvalues(int no) {
    IntegerVector result(no);
    int* r = result.begin();
    double r2;
    int ii;
    for( int i = 0; i < no; ++i) {
        ii = m_values[m_unif_int_distribution(m_generator)];
        r2 = m_unif_double_distribution(m_generator);
        r[i] = (r2 < m_prob[ii]) ? ii : m_alias[ii];
    }
    return result;
}

ex Finitization::ntsf(symbol x, ex pnb) {
    return series_to_poly(pnb.series(x == 0, m_finitizationOrder+1));

}

ex Finitization::pdf(symbol x, symbol _theta, ex ntsf, int x_val) {
    ex optheta = -_theta;
    ex pdf = (ntsf.diff(x, x_val));
    pdf = pdf.subs(x == optheta) * pow(_theta, x_val) / factorial(x_val);
    return pdf;

}
