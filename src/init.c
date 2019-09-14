#include "dateutils.h"
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

static const
R_CallMethodDef callMethods[] = {
	{"Cyear.EDate", (DL_FUNC)&year_EDate, -1},
	{"Cyear<-", (DL_FUNC)&year_bang, -1},
	{"Cyday.EDate", (DL_FUNC)&yday_EDate, -1},
	{"Cyday<-", (DL_FUNC)&yday_bang, -1},
	{"Cmonth.EDate", (DL_FUNC)&month_EDate, -1},
	{"Cmonth<-", (DL_FUNC)&month_bang, -1},
	{"Cmday.EDate", (DL_FUNC)&mday_EDate, -1},
	{"Cmday<-", (DL_FUNC)&mday_bang, -1},
	{"Cas.POSIXlt.EDate", (DL_FUNC)&as_POSIXlt_EDate, -1},

	{"Cas.FDate.character", (DL_FUNC)&as_FDate_character, -1},
	{"Cformat.FDate", (DL_FUNC)&format_FDate, -1},
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

	{NULL, NULL, 0}
};

static const
R_ExternalMethodDef externalMethods[] = {
	{NULL, NULL, 0}
};

void attribute_visible R_init_dateutils(DllInfo *info)
{
	R_registerRoutines(info, NULL, callMethods, NULL, externalMethods);
	return;
}
