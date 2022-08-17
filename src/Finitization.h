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

    string pdfToString(int val, bool tolatex = false);
    IntegerVector rvalues(int no );
    double fin_pdf(int val);

protected:
    void setProbs(double *probs);
    ex ntsf(ex pnb);
    ex pdf(ex ntsf, int x_val);

    ex fin_pdfSymb(int x_val);
    virtual ex ntsd_base(symbol x, symbol theta) = 0;

    int m_finitizationOrder;
    double m_theta;
    symbol m_paramSymb;
    symbol m_x;
    double* m_dprobs;
    bool m_finish;


private:

	std::uniform_real_distribution<double> m_unif_double_distribution;
    std::uniform_int_distribution<int> m_unif_int_distribution;
	std::mt19937 m_generator;
    int*  m_alias;
    double* m_prob;
    int* m_values;
    bool m_ntsfFirstTime;
    ex m_ntsfSymb;
    std::unordered_map<int, ex> m_cache;


};
#endif /* FINITIZATION_H_ */
