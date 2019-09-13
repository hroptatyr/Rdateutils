#include <R.h>
#include <Rinternals.h>

typedef unsigned int EDate;
typedef unsigned int FDate;

SEXP year_EDate(SEXP x);
SEXP year_bang(SEXP x, SEXP value);
SEXP yday_EDate(SEXP x);
SEXP yday_bang(SEXP x, SEXP value);
SEXP month_EDate(SEXP x);
SEXP month_bang(SEXP x, SEXP value);
SEXP mday_EDate(SEXP x);
SEXP mday_bang(SEXP x, SEXP value);

SEXP as_POSIXlt_EDate(SEXP x);

SEXP CFDate(SEXP y, SEXP m, SEXP d);
SEXP as_FDate_character(SEXP x);
SEXP format_FDate(SEXP x);
SEXP as_POSIXlt_FDate(SEXP x);

SEXP year_FDate(SEXP x);
SEXP yday_FDate(SEXP x);
SEXP semi_FDate(SEXP x);
SEXP sday_FDate(SEXP x);
SEXP quarter_FDate(SEXP x);
SEXP qday_FDate(SEXP x);
SEXP month_FDate(SEXP x);
SEXP mday_FDate(SEXP x);
SEXP week_FDate(SEXP x);
SEXP wday_FDate(SEXP x);
