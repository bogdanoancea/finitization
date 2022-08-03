// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// rpois
IntegerVector rpois(int n, double theta, unsigned no);
RcppExport SEXP _finitization_rpois(SEXP nSEXP, SEXP thetaSEXP, SEXP noSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< double >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< unsigned >::type no(noSEXP);
    rcpp_result_gen = Rcpp::wrap(rpois(n, theta, no));
    return rcpp_result_gen;
END_RCPP
}
// dpois
double dpois(int n, double theta, double val);
RcppExport SEXP _finitization_dpois(SEXP nSEXP, SEXP thetaSEXP, SEXP valSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< double >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< double >::type val(valSEXP);
    rcpp_result_gen = Rcpp::wrap(dpois(n, theta, val));
    return rcpp_result_gen;
END_RCPP
}
// printFinitizedPoissonDensity
SEXP printFinitizedPoissonDensity(int n, int val, bool latex);
RcppExport SEXP _finitization_printFinitizedPoissonDensity(SEXP nSEXP, SEXP valSEXP, SEXP latexSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type val(valSEXP);
    Rcpp::traits::input_parameter< bool >::type latex(latexSEXP);
    rcpp_result_gen = Rcpp::wrap(printFinitizedPoissonDensity(n, val, latex));
    return rcpp_result_gen;
END_RCPP
}
// printFinitizedPoissonDensity2
String printFinitizedPoissonDensity2(int n, int val);
RcppExport SEXP _finitization_printFinitizedPoissonDensity2(SEXP nSEXP, SEXP valSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type val(valSEXP);
    rcpp_result_gen = Rcpp::wrap(printFinitizedPoissonDensity2(n, val));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_finitization_rpois", (DL_FUNC) &_finitization_rpois, 3},
    {"_finitization_dpois", (DL_FUNC) &_finitization_dpois, 3},
    {"_finitization_printFinitizedPoissonDensity", (DL_FUNC) &_finitization_printFinitizedPoissonDensity, 3},
    {"_finitization_printFinitizedPoissonDensity2", (DL_FUNC) &_finitization_printFinitizedPoissonDensity2, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_finitization(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
