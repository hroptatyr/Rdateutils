#include <R.h>
#include <Rinternals.h>

typedef unsigned int EDate;
typedef unsigned int FDate;

SEXP year(SEXP x);
SEXP year_bang(SEXP x, SEXP value);
SEXP yday(SEXP x);
SEXP yday_bang(SEXP x, SEXP value);
SEXP month(SEXP x);
SEXP month_bang(SEXP x, SEXP value);
SEXP mday(SEXP x);
SEXP mday_bang(SEXP x, SEXP value);

SEXP as_POSIXlt_EDate(SEXP x);

SEXP CFDate(SEXP y, SEXP m, SEXP d);
SEXP as_FDate_character(SEXP x);
SEXP format_FDate(SEXP x);
SEXP as_POSIXlt_FDate(SEXP x);
SEXP fmd(SEXP x);
