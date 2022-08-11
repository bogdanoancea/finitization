#include <random>
#include "Rcpp.h"
#include <ginac/ginac.h>

using namespace std;
using namespace Rcpp;
using namespace GiNaC;

#ifndef FINITIZATION_H_
#define FINITIZATION_H_
class Finitization {

public:
    Finitization(int n);

    virtual ~Finitization();

    virtual string pdfToString(int val, bool tolatex = false) = 0;
    virtual double fin_pdf(int val)= 0;
    IntegerVector rvalues(int no );

protected:
    void setProbs(double *probs);
    ex ntsf(symbol x, ex pnb);
    ex pdf(symbol x, symbol _theta, ex ntsf, int x_val);
    virtual ex ntsd_base(symbol x, symbol theta) = 0;
    int m_finitizationOrder;

private:

	std::uniform_real_distribution<double> m_unif_double_distribution;
    std::uniform_int_distribution<int> m_unif_int_distribution;
	std::mt19937 m_generator;
    int*  m_alias;
    double* m_prob;
    int* m_values;


};
#endif /* FINITIZATION_H_ */
