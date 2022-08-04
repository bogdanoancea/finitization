#include <random>
#include "Rcpp.h"
using namespace std;
using namespace Rcpp;

#ifndef FINITIZATION_H_
#define FINITIZATION_H_
class Finitization {

public:
    Finitization(int n);

    virtual ~Finitization();

    virtual string pdfToString(int val, bool latex = false) = 0;
    virtual double fin_pdf(int val)= 0;
    IntegerVector rvalues(int no );
    //int rvalue();
    virtual double getProb(int val)  = 0;
    virtual double getMFPSUL();

protected:
    void setProbs(double *probs);
    virtual double fin_pdf(int val, double theta) = 0;

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
