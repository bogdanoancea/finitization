#include <random>
#include "Rcpp.h"
using namespace std;
using namespace Rcpp;


class Finitization {

public:
    Finitization(int n);

    virtual ~Finitization();

    virtual string pdfToString(int val, bool latex = false) = 0;
    virtual double fin_pdf(int val)= 0;
    IntegerVector rvalues(int no );
    int rvalue();
    virtual double getProb(int val)  = 0;


protected:
    int m_finitizationOrder;
    void setProbs(double *probs);
private:

	std::uniform_real_distribution<double> m_unif_double_distribution;
    std::uniform_int_distribution<int> m_unif_int_distribution;
	std::mt19937 m_generator;
    int*  m_alias;
    double* m_prob;
    int* m_values;


};
