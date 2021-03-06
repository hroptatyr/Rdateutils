#include "dateutils.h"
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

static const
R_CallMethodDef callMethods[] = {
	{"Cas.FDate.character", (DL_FUNC)&as_FDate_character, -1},
	{"Cas.FDate.factor", (DL_FUNC)&as_FDate_factor, -1},
	{"Cas.FDate.integer", (DL_FUNC)&as_FDate_integer, -1},
	{"Cas.FDate.IDate", (DL_FUNC)&as_FDate_IDate, -1},
	{"Cformat.FDate", (DL_FUNC)&format_FDate, -1},
	{"Cas.IDate.FDate", (DL_FUNC)&as_IDate_FDate, -1},
	{"Cas.POSIXlt.FDate", (DL_FUNC)&as_POSIXlt_FDate, -1},

	{"Cyear.FDate", (DL_FUNC)&year_FDate, -1},
	{"Cyday.FDate", (DL_FUNC)&yday_FDate, -1},
	{"Csemi.FDate", (DL_FUNC)&semi_FDate, -1},
	{"Csday.FDate", (DL_FUNC)&sday_FDate, -1},
	{"Cquarter.FDate", (DL_FUNC)&quarter_FDate, -1},
	{"Cqday.FDate", (DL_FUNC)&qday_FDate, -1},
	{"Cmonth.FDate", (DL_FUNC)&month_FDate, -1},
	{"Cmday.FDate", (DL_FUNC)&mday_FDate, -1},
	{"Cweek.FDate", (DL_FUNC)&week_FDate, -1},
	{"Cwday.FDate", (DL_FUNC)&wday_FDate, -1},
	{"Csweek.FDate", (DL_FUNC)&sweek_FDate, -1},
	{"Cqweek.FDate", (DL_FUNC)&qweek_FDate, -1},
	{"Cmweek.FDate", (DL_FUNC)&mweek_FDate, -1},
	{"Cwcnt.FDate", (DL_FUNC)&wcnt_FDate, -1},
	{"Cswcnt.FDate", (DL_FUNC)&swcnt_FDate, -1},
	{"Cqwcnt.FDate", (DL_FUNC)&qwcnt_FDate, -1},
	{"Cmwcnt.FDate", (DL_FUNC)&mwcnt_FDate, -1},
	{"Cyear<-.FDate", (DL_FUNC)&year_bang_FDate, -1},
	{"Cyday<-.FDate", (DL_FUNC)&yday_bang_FDate, -1},
	{"Csday<-.FDate", (DL_FUNC)&sday_bang_FDate, -1},
	{"Cqday<-.FDate", (DL_FUNC)&qday_bang_FDate, -1},
	{"Cmonth<-.FDate", (DL_FUNC)&month_bang_FDate, -1},
	{"Cmday<-.FDate", (DL_FUNC)&mday_bang_FDate, -1},
	{"Cweek<-.FDate", (DL_FUNC)&week_bang_FDate, -1},
	{"Cwday<-.FDate", (DL_FUNC)&wday_bang_FDate, -1},

	{"Cnyday.FDate", (DL_FUNC)&nyday_FDate, -1},
	{"Cnsday.FDate", (DL_FUNC)&nsday_FDate, -1},
	{"Cnqday.FDate", (DL_FUNC)&nqday_FDate, -1},
	{"Cnmday.FDate", (DL_FUNC)&nmday_FDate, -1},

	{"Ctrunc.FDate.year", (DL_FUNC)&trunc_FDate_year, -1},
	{"Ctrunc.FDate.semi", (DL_FUNC)&trunc_FDate_semi, -1},
	{"Ctrunc.FDate.quarter", (DL_FUNC)&trunc_FDate_quarter, -1},
	{"Ctrunc.FDate.month", (DL_FUNC)&trunc_FDate_month, -1},

	{"Cbegd.FDate", (DL_FUNC)&begd_FDate, -1},
	{"Cendd.FDate", (DL_FUNC)&endd_FDate, -1},
	{"Cpdur.FDate", (DL_FUNC)&pdur_FDate, -1},

	{"Cas.ddur.character", (DL_FUNC)&as_ddur_character, -1},
	{"Cas.ddur.factor", (DL_FUNC)&as_ddur_factor, -1},
	{"Cas.ddur.numeric", (DL_FUNC)&as_ddur_numeric, -1},
	{"Cas.ddur.wcnt", (DL_FUNC)&as_ddur_wcnt, -1},
	{"Cis.na.ddur", (DL_FUNC)&is_na_ddur, -1},
	{"Cformat.ddur", (DL_FUNC)&format_ddur, -1},
	{"C+.ddur", (DL_FUNC)&plus_ddur, -1},
	{"Cneg.ddur", (DL_FUNC)&neg_ddur, -1},
	{"Cseq.ddur", (DL_FUNC)&seq_ddur, -1},
	{"C*.ddur", (DL_FUNC)&mul_ddur, -1},
	{"C/.ddur", (DL_FUNC)&div_ddur, -1},
	{"C%.ddur", (DL_FUNC)&mod_ddur, -1},
	{"C<.ddur", (DL_FUNC)&lt_ddur, -1},
	{"C<=.ddur", (DL_FUNC)&le_ddur, -1},
	{"C>.ddur", (DL_FUNC)&gt_ddur, -1},
	{"C>=.ddur", (DL_FUNC)&ge_ddur, -1},

	{"Ctrunc.ddur.year", (DL_FUNC)&trunc_ddur_year, -1},
	{"Ctrunc.ddur.semi", (DL_FUNC)&trunc_ddur_semi, -1},
	{"Ctrunc.ddur.quarter", (DL_FUNC)&trunc_ddur_quarter, -1},
	{"Ctrunc.ddur.month", (DL_FUNC)&trunc_ddur_month, -1},
	{"Ctrunc.ddur.week", (DL_FUNC)&trunc_ddur_week, -1},
	{"Ctrunc.ddur.day", (DL_FUNC)&trunc_ddur_day, -1},

	{"Cyear.ddur", (DL_FUNC)&year_ddur, -1},
	{"Csemi.ddur", (DL_FUNC)&semi_ddur, -1},
	{"Cquarter.ddur", (DL_FUNC)&quarter_ddur, -1},
	{"Cmonth.ddur", (DL_FUNC)&month_ddur, -1},
	{"Cweek.ddur", (DL_FUNC)&week_ddur, -1},
	{"Cdday.ddur", (DL_FUNC)&dday_ddur, -1},
	{"Cyear<-.ddur", (DL_FUNC)&year_bang_ddur, -1},
	{"Cmonth<-.ddur", (DL_FUNC)&month_bang_ddur, -1},
	{"Cweek<-.ddur", (DL_FUNC)&week_bang_ddur, -1},
	{"Cdday<-.ddur", (DL_FUNC)&dday_bang_ddur, -1},

	{"C+.FDate", (DL_FUNC)&plus_FDate, -1},
	{"C-.FDate", (DL_FUNC)&minus_FDate, -1},
	{"Cddur.FDate", (DL_FUNC)&ddur_FDate, -1},
	{"Cseq.FDate", (DL_FUNC)&seq_FDate, -1},

	{"Cas.wcnt.character", (DL_FUNC)&as_wcnt_character, -1},
	{"Cas.wcnt.factor", (DL_FUNC)&as_wcnt_factor, -1},
	{"Cformat.wcnt", (DL_FUNC)&format_wcnt, -1},
	{"C+.wcnt", (DL_FUNC)&plus_wcnt, -1},
	{"Cweek.wcnt", (DL_FUNC)&week_wcnt, -1},
	{"Cwday.wcnt", (DL_FUNC)&wday_wcnt, -1},
	{"Cweek<-.wcnt", (DL_FUNC)&week_bang_wcnt, -1},
	{"Cwday<-.wcnt", (DL_FUNC)&wday_bang_wcnt, -1},
	{NULL, NULL, 0}
};

static const
R_ExternalMethodDef externalMethods[] = {
	{NULL, NULL, 0}
};

void attribute_visible R_init_dateutils(DllInfo *info)
{
	R_registerRoutines(info, NULL, callMethods, NULL, externalMethods);
	R_useDynamicSymbols(info, FALSE);
	return;
}
