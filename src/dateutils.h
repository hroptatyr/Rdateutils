#include <R.h>
#include <Rinternals.h>

typedef unsigned int EDate;

SEXP year(SEXP x);
SEXP yday(SEXP x);
SEXP month(SEXP x);
SEXP mday(SEXP x);

SEXP as_POSIXlt_EDate(SEXP x);
