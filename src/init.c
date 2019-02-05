#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _EasyMMD_kernelMatrix_sum(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _EasyMMD_kernelMatrix_sum_multi(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _EasyMMD_kernelMatrix_threshold_sum(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_EasyMMD_kernelMatrix_sum",           (DL_FUNC) &_EasyMMD_kernelMatrix_sum,           6},
    {"_EasyMMD_kernelMatrix_sum_multi",     (DL_FUNC) &_EasyMMD_kernelMatrix_sum_multi,     6},
    {"_EasyMMD_kernelMatrix_threshold_sum", (DL_FUNC) &_EasyMMD_kernelMatrix_threshold_sum, 7},
    {NULL, NULL, 0}
};

void R_init_EasyMMD(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
