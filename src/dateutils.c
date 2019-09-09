#include <unistd.h>
#include <sys/socket.h>
#include <sys/epoll.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>
#include <string.h>
#include <R.h>
#include "dateutils.h"
#include "nifty.h"

static inline EDate
_j00(unsigned int y)
{
	return y * 365U + y / 4U - y / 100U + y / 400U;
}

static inline unsigned int
_year(EDate x)
{
	unsigned int guess = x / 365U;
	guess -= _j00(guess) > x;
	guess -= _j00(guess) > x;
	/* more corrections in the year 3000 or so */
	return guess;
}

static inline unsigned int
_yday(EDate x)
{
	unsigned int y = _year(x);
	return x - _j00(y) + 1U;
}

static inline unsigned int
_yyd(EDate x)
{
	unsigned int y = _year(x);
	unsigned int yd = x - _j00(y) + 1U;
	return (y << 16U) ^ yd;
}

static inline unsigned int
_month(EDate x)
{
	unsigned int yd = _yday(x) - 1;
	unsigned int pent = yd / 153U;
	unsigned int pend = yd % 153U;
	unsigned int mo = (2U * pend / 61U);
	return 5U * pent + mo + 1U;
}

static inline unsigned int
_mday(EDate x)
{
	unsigned int yd = _yday(x) - 1U;
	unsigned int pend = yd % 153U;
	unsigned int md = (2U * pend % 61U) / 2U;
	return md + 1U;
}

static inline unsigned int
_momd(unsigned int yd)
{
/* YD is actually 0-based yday */
	unsigned int pent = yd / 153U;
	unsigned int pend = yd % 153U;
	unsigned int mo = (2U * pend / 61U);
	unsigned int md = (2U * pend % 61U) / 2U;
	return (5U * pent + mo + 1U) << 8U ^ (md + 1U);
}

static inline int
_leapp(unsigned int y)
{
	return !((y % 4U) || !(y % 100U) && (y % 400U));
}


SEXP
year(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans;

	PROTECT(ans = allocVector(INTSXP, n));

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = INTEGER(x)[i];
		unsigned int yyd = _yyd(m);
		unsigned int y = (yyd >> 16U);
		unsigned int yd = yyd & 0xffffU;

		/* massage y and yd into Jan years */
		y += yd >= 307U;
		INTEGER(ans)[i] = m != NA_INTEGER ? y : NA_INTEGER;
	}

	UNPROTECT(1);
	return ans;
}

SEXP
year_bang(SEXP x, SEXP value)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans;

	PROTECT(ans = allocVector(INTSXP, n));

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = INTEGER(x)[i];
		int y2b = INTEGER(value)[i];
		unsigned int yyd = _yyd(m);
		unsigned int y = (yyd >> 16U);
		unsigned int yd = yyd & 0xffffU;

		/* massage y and yd into Jan years */
		yd -= yd > 365U && !_leapp(y2b);
		y2b -= yd >= 307U;
		INTEGER(ans)[i] = m != NA_INTEGER ? _j00(y2b) + yd - 1 : NA_INTEGER;
	}

	with (SEXP class) {
		PROTECT(class = allocVector(STRSXP, 1));
		SET_STRING_ELT(class, 0, mkChar("EDate"));
		classgets(ans, class);
	}

	UNPROTECT(2);
	return ans;
}

SEXP
yday(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans;

	PROTECT(ans = allocVector(INTSXP, n));

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = INTEGER(x)[i];
		unsigned int yyd = _yyd(m);
		unsigned int y = (yyd >> 16U);
		unsigned int yd = yyd & 0xffffU;

		/* massage y and yd into Jan years */
		y += yd >= 307U;
		yd += yd < 307U ? 59U + _leapp(y) : -306U;
		INTEGER(ans)[i] = m != NA_INTEGER ? yd : NA_INTEGER;
	}

	UNPROTECT(1);
	return ans;
}

SEXP
yday_bang(SEXP x, SEXP value)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans;

	PROTECT(ans = allocVector(INTSXP, n));

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = INTEGER(x)[i];
		int yd2b = INTEGER(value)[i];
		unsigned int yyd = _yyd(m);
		unsigned int y = (yyd >> 16U);
		unsigned int yd = yyd & 0xffffU;

		/* massage y into Jan years */
		y += yd >= 307U;
		/* clamp between -366/366 */
		yd2b = yd2b <= 365 ? yd2b : !_leapp(y) ? 365 : 366;
		yd2b = yd2b >= -364 ? yd2b : !_leapp(y-1) ? -364 : -365;
		yd2b -= 59 + _leapp(y);
		INTEGER(ans)[i] = m != NA_INTEGER ? _j00(y) + yd2b - 1 : NA_INTEGER;
	}

	with (SEXP class) {
		PROTECT(class = allocVector(STRSXP, 1));
		SET_STRING_ELT(class, 0, mkChar("EDate"));
		classgets(ans, class);
	}

	UNPROTECT(2);
	return ans;
}

SEXP
month(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans;

	PROTECT(ans = allocVector(INTSXP, n));

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = INTEGER(x)[i];
		INTEGER(ans)[i] = m != NA_INTEGER ? _month(m) : NA_INTEGER;
	}

	UNPROTECT(1);
	return ans;
}

SEXP
mday(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans;

	PROTECT(ans = allocVector(INTSXP, n));

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = INTEGER(x)[i];
		INTEGER(ans)[i] = m != NA_INTEGER ? _mday(m) : NA_INTEGER;
	}

	UNPROTECT(1);
	return ans;
}

SEXP
as_POSIXlt_EDate(SEXP x)
{
	static const char ltnames [][7] = {"sec", "min", "hour", "mday", "mon", "year", "wday", "yday", "isdst", "zone",  "gmtoff"};
	R_xlen_t n = XLENGTH(x);
	SEXP ans;

	PROTECT(ans = allocVector(VECSXP, 9));
	SET_VECTOR_ELT(ans, 0, allocVector(REALSXP, n));
	for (int i = 1; i < 9; i++) {
		SET_VECTOR_ELT(ans, i, allocVector(INTSXP, n));
	}

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = INTEGER(x)[i];

		if (m != NA_INTEGER) {
			unsigned int yyd = _yyd(m);
			unsigned int y = (yyd >> 16U);
			unsigned int yd = yyd & 0xffffU;
			unsigned int momd = _momd(yd - 1);
			unsigned int mo = momd >> 8U;
			unsigned int md = momd & 0xffU;

			/* massage y and yd into Jan years */
			y += yd >= 307U;
			yd += yd < 307U ? 58U + _leapp(y) : -307U;

			REAL(VECTOR_ELT(ans, 0))[i] = 0.;
			INTEGER(VECTOR_ELT(ans, 1))[i] = 0;
			INTEGER(VECTOR_ELT(ans, 2))[i] = 0;
			/* mday */
			INTEGER(VECTOR_ELT(ans, 3))[i] = md;
			/* mon */
			INTEGER(VECTOR_ELT(ans, 4))[i] = (mo + 1U) % 12U;
			/* year */
			INTEGER(VECTOR_ELT(ans, 5))[i] = y - 1900;
			/* wday */
			INTEGER(VECTOR_ELT(ans, 6))[i] = (m + 3) % 7;
			/* yday */
			INTEGER(VECTOR_ELT(ans, 7))[i] = yd;
			/* is dst */
			INTEGER(VECTOR_ELT(ans, 8))[i] = -1;
		} else {
			REAL(VECTOR_ELT(ans, 0))[i] = NA_REAL;
			INTEGER(VECTOR_ELT(ans, 1))[i] = NA_INTEGER;
			INTEGER(VECTOR_ELT(ans, 2))[i] = NA_INTEGER;
			/* mday */
			INTEGER(VECTOR_ELT(ans, 3))[i] = NA_INTEGER;
			/* mon */
			INTEGER(VECTOR_ELT(ans, 4))[i] = NA_INTEGER;
			/* year */
			INTEGER(VECTOR_ELT(ans, 5))[i] = NA_INTEGER;
			/* wday */
			INTEGER(VECTOR_ELT(ans, 6))[i] = NA_INTEGER;
			/* yday */
			INTEGER(VECTOR_ELT(ans, 7))[i] = NA_INTEGER;
			/* is dst */
			INTEGER(VECTOR_ELT(ans, 8))[i] = -1;
		}
	}

	with (SEXP names) {
		PROTECT(names = allocVector(STRSXP, 9));
		for (int i = 0; i < 9; i++) {
			SET_STRING_ELT(names, i, mkChar(ltnames[i]));
		}
		namesgets(ans, names);
	}

	with (SEXP class) {
		PROTECT(class = allocVector(STRSXP, 2));
		SET_STRING_ELT(class, 0, mkChar("POSIXlt"));
		SET_STRING_ELT(class, 1, mkChar("POSIXt"));
		classgets(ans, class);
	}

	UNPROTECT(3);
	return ans;
}
