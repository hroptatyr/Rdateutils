#include <R.h>
#include <Rinternals.h>

SEXP as_FDate_character(SEXP x);
SEXP as_FDate_factor(SEXP x);
SEXP as_FDate_integer(SEXP x);
SEXP as_FDate_IDate(SEXP x);
SEXP format_FDate(SEXP x);
SEXP as_IDate_FDate(SEXP x);
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
SEXP sweek_FDate(SEXP x);
SEXP qweek_FDate(SEXP x);
SEXP mweek_FDate(SEXP x);
SEXP wcnt_FDate(SEXP x);
SEXP swcnt_FDate(SEXP x);
SEXP qwcnt_FDate(SEXP x);
SEXP mwcnt_FDate(SEXP x);

SEXP nyday_FDate(SEXP x);
SEXP nsday_FDate(SEXP x);
SEXP nqday_FDate(SEXP x);
SEXP nmday_FDate(SEXP x);

SEXP year_bang_FDate(SEXP x, SEXP value);
SEXP yday_bang_FDate(SEXP x, SEXP value);
SEXP sday_bang_FDate(SEXP x, SEXP value);
SEXP qday_bang_FDate(SEXP x, SEXP value);
SEXP month_bang_FDate(SEXP x, SEXP value);
SEXP mday_bang_FDate(SEXP x, SEXP value);
SEXP week_bang_FDate(SEXP x, SEXP value);
SEXP wday_bang_FDate(SEXP x, SEXP value);

SEXP trunc_FDate_year(SEXP x);
SEXP trunc_FDate_semi(SEXP x);
SEXP trunc_FDate_quarter(SEXP x);
SEXP trunc_FDate_month(SEXP x);

SEXP begd_FDate(SEXP x);
SEXP endd_FDate(SEXP x);
SEXP pdur_FDate(SEXP x);

SEXP seq_FDate(SEXP from, SEXP till, SEXP by);

SEXP as_ddur_character(SEXP x);
SEXP as_ddur_factor(SEXP x);
SEXP as_ddur_numeric(SEXP x);
SEXP as_ddur_wcnt(SEXP x);
SEXP is_na_ddur(SEXP x);
SEXP format_ddur(SEXP x);
SEXP plus_ddur(SEXP x, SEXP y);
SEXP neg_ddur(SEXP x);
SEXP mul_ddur(SEXP x, SEXP y);
SEXP div_ddur(SEXP x, SEXP y);
SEXP mod_ddur(SEXP x, SEXP y);
SEXP lt_ddur(SEXP x, SEXP y);
SEXP le_ddur(SEXP x, SEXP y);
SEXP gt_ddur(SEXP x, SEXP y);
SEXP ge_ddur(SEXP x, SEXP y);

SEXP trunc_ddur_year(SEXP x);
SEXP trunc_ddur_semi(SEXP x);
SEXP trunc_ddur_quarter(SEXP x);
SEXP trunc_ddur_month(SEXP x);
SEXP trunc_ddur_week(SEXP x);
SEXP trunc_ddur_day(SEXP x);

SEXP year_ddur(SEXP x);
SEXP semi_ddur(SEXP x);
SEXP quarter_ddur(SEXP x);
SEXP month_ddur(SEXP x);
SEXP week_ddur(SEXP x);
SEXP dday_ddur(SEXP x);
SEXP year_bang_ddur(SEXP x, SEXP value);
SEXP month_bang_ddur(SEXP x, SEXP value);
SEXP week_bang_ddur(SEXP x, SEXP value);
SEXP dday_bang_ddur(SEXP x, SEXP value);

SEXP seq_ddur(SEXP from, SEXP till, SEXP by);

SEXP plus_FDate(SEXP x, SEXP y);
SEXP minus_FDate(SEXP x, SEXP y);
SEXP ddur_FDate(SEXP x, SEXP y);

SEXP as_wcnt_character(SEXP x);
SEXP as_wcnt_factor(SEXP x);
SEXP format_wcnt(SEXP x);
SEXP plus_wcnt(SEXP x, SEXP y);
SEXP week_wcnt(SEXP x);
SEXP wday_wcnt(SEXP x);
SEXP week_bang_wcnt(SEXP x, SEXP value);
SEXP wday_bang_wcnt(SEXP x, SEXP value);
