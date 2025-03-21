// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// c_printDensity
StringVector c_printDensity(int n, IntegerVector val, Rcpp::List const& params, int dtype, bool latex);
RcppExport SEXP _finitization_c_printDensity(SEXP nSEXP, SEXP valSEXP, SEXP paramsSEXP, SEXP dtypeSEXP, SEXP latexSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type val(valSEXP);
    Rcpp::traits::input_parameter< Rcpp::List const& >::type params(paramsSEXP);
    Rcpp::traits::input_parameter< int >::type dtype(dtypeSEXP);
    Rcpp::traits::input_parameter< bool >::type latex(latexSEXP);
    rcpp_result_gen = Rcpp::wrap(c_printDensity(n, val, params, dtype, latex));
    return rcpp_result_gen;
END_RCPP
}
// c_d
NumericVector c_d(int n, IntegerVector val, Rcpp::List const& params, int dtype);
RcppExport SEXP _finitization_c_d(SEXP nSEXP, SEXP valSEXP, SEXP paramsSEXP, SEXP dtypeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type val(valSEXP);
    Rcpp::traits::input_parameter< Rcpp::List const& >::type params(paramsSEXP);
    Rcpp::traits::input_parameter< int >::type dtype(dtypeSEXP);
    rcpp_result_gen = Rcpp::wrap(c_d(n, val, params, dtype));
    return rcpp_result_gen;
END_RCPP
}
// rvalues
IntegerVector rvalues(int n, Rcpp::List const& params, int no, int dtype);
RcppExport SEXP _finitization_rvalues(SEXP nSEXP, SEXP paramsSEXP, SEXP noSEXP, SEXP dtypeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< Rcpp::List const& >::type params(paramsSEXP);
    Rcpp::traits::input_parameter< int >::type no(noSEXP);
    Rcpp::traits::input_parameter< int >::type dtype(dtypeSEXP);
    rcpp_result_gen = Rcpp::wrap(rvalues(n, params, no, dtype));
    return rcpp_result_gen;
END_RCPP
}
// MFPS_pdf
String MFPS_pdf(int n, Rcpp::List const& params, int dtype);
RcppExport SEXP _finitization_MFPS_pdf(SEXP nSEXP, SEXP paramsSEXP, SEXP dtypeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< Rcpp::List const& >::type params(paramsSEXP);
    Rcpp::traits::input_parameter< int >::type dtype(dtypeSEXP);
    rcpp_result_gen = Rcpp::wrap(MFPS_pdf(n, params, dtype));
    return rcpp_result_gen;
END_RCPP
}
// getPoissonType
int getPoissonType();
RcppExport SEXP _finitization_getPoissonType() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(getPoissonType());
    return rcpp_result_gen;
END_RCPP
}
// getNegativeBinomialType
int getNegativeBinomialType();
RcppExport SEXP _finitization_getNegativeBinomialType() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(getNegativeBinomialType());
    return rcpp_result_gen;
END_RCPP
}
// getBinomialType
int getBinomialType();
RcppExport SEXP _finitization_getBinomialType() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(getBinomialType());
    return rcpp_result_gen;
END_RCPP
}
// getLogarithmicType
int getLogarithmicType();
RcppExport SEXP _finitization_getLogarithmicType() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(getLogarithmicType());
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_finitization_c_printDensity", (DL_FUNC) &_finitization_c_printDensity, 5},
    {"_finitization_c_d", (DL_FUNC) &_finitization_c_d, 4},
    {"_finitization_rvalues", (DL_FUNC) &_finitization_rvalues, 4},
    {"_finitization_MFPS_pdf", (DL_FUNC) &_finitization_MFPS_pdf, 3},
    {"_finitization_getPoissonType", (DL_FUNC) &_finitization_getPoissonType, 0},
    {"_finitization_getNegativeBinomialType", (DL_FUNC) &_finitization_getNegativeBinomialType, 0},
    {"_finitization_getBinomialType", (DL_FUNC) &_finitization_getBinomialType, 0},
    {"_finitization_getLogarithmicType", (DL_FUNC) &_finitization_getLogarithmicType, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_finitization(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
