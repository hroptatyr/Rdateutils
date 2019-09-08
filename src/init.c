#include "dateutils.h"
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

static const
R_CallMethodDef callMethods[] = {
	{"Cyear", (DL_FUNC)&year, -1},
	{"Cyday", (DL_FUNC)&yday, -1},
	{"Cmonth", (DL_FUNC)&month, -1},
	{"Cmday", (DL_FUNC)&mday, -1},
	{"Cas.POSIXlt.EDate", (DL_FUNC)&as_POSIXlt_EDate, -1},
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
