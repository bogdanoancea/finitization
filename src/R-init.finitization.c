#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP _finitization_c_d(SEXP, SEXP, SEXP, SEXP);
extern SEXP _finitization_c_printDensity(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _finitization_check_symbolic_equivalence(SEXP, SEXP);
extern SEXP _finitization_getBinomialType(void);
extern SEXP _finitization_getLogarithmicType(void);
extern SEXP _finitization_getNegativeBinomialType(void);
extern SEXP _finitization_getPoissonType(void);
extern SEXP _finitization_MFPS_pdf(SEXP, SEXP, SEXP);
extern SEXP _finitization_rvalues(SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_finitization_c_d",                        (DL_FUNC) &_finitization_c_d,                        4},
    {"_finitization_c_printDensity",             (DL_FUNC) &_finitization_c_printDensity,             5},
    {"_finitization_check_symbolic_equivalence", (DL_FUNC) &_finitization_check_symbolic_equivalence, 2},
    {"_finitization_getBinomialType",            (DL_FUNC) &_finitization_getBinomialType,            0},
    {"_finitization_getLogarithmicType",         (DL_FUNC) &_finitization_getLogarithmicType,         0},
    {"_finitization_getNegativeBinomialType",    (DL_FUNC) &_finitization_getNegativeBinomialType,    0},
    {"_finitization_getPoissonType",             (DL_FUNC) &_finitization_getPoissonType,             0},
    {"_finitization_MFPS_pdf",                   (DL_FUNC) &_finitization_MFPS_pdf,                   3},
    {"_finitization_rvalues",                    (DL_FUNC) &_finitization_rvalues,                    4},
    {NULL, NULL, 0}
};

void R_init_finitization(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
