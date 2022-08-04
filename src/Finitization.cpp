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
    int *S = new int[m_finitizationOrder+1];
    int *L = new int[m_finitizationOrder+1];
    double *P = new double[m_finitizationOrder+1];

    double sum=0.0;
    for (i = 0; i < m_finitizationOrder+1; ++i ) {
        sum += p[i];
    }
    for (i = 0; i < m_finitizationOrder+1; ++i )
        P[i] = p[i] * (m_finitizationOrder+1) / sum;

    int nS = 0, nL = 0;
    for (i = m_finitizationOrder; i>=0; --i ) {
        if ( P[i]<1 )
            S[nS++] = i;
        else
            L[nL++] = i;
    }
    while ( nS && nL ) {
        a = S[--nS];
        g = L[--nL];
        m_prob[a] = P[a];
        m_alias[a] = g;
        P[g] = P[g] + P[a] - 1;
        if ( P[g] < 1 )
            S[nS++] = g;
        else
            L[nL++] = g;
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


// int  Finitization::rvalue()  {
//     double r2;
//     int ii;
//     ii = m_values[m_unif_int_distribution(m_generator)];
//     r2 = m_unif_double_distribution(m_generator);
//     return (r2 < m_prob[ii]) ? ii : m_alias[ii];
//
// }

double Finitization::getMFPSUL() {

    double x0 =  1 - 2 * std::numeric_limits<double>::epsilon();
    const double error = 1e-8;
    const double h = std::numeric_limits<double>::epsilon();


    double fx0 = fin_pdf(m_finitizationOrder - 1, x0);
    int i{0};
    while(std::fabs(fx0) > error){
        double diff = (fin_pdf(m_finitizationOrder - 1, x0 + h) - fx0)/ h ;
        x0 = x0 - fx0/diff;
        fx0 = fin_pdf(m_finitizationOrder - 1, x0);
        i++;
    }
    return x0;
}

