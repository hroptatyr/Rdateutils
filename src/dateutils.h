#include <R.h>
#include <Rinternals.h>

typedef unsigned int EDate;

SEXP year(SEXP x);
SEXP year_bang(SEXP x, SEXP value);
SEXP yday(SEXP x);
SEXP yday_bang(SEXP x, SEXP value);
SEXP month(SEXP x);
SEXP month_bang(SEXP x, SEXP value);
SEXP mday(SEXP x);
SEXP mday_bang(SEXP x, SEXP value);

SEXP as_POSIXlt_EDate(SEXP x);
