#include <unistd.h>
#include <stdint.h>
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
	return (5U * pent + mo + 2U) % 12U + 1U;
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


/* Financial calendar
 * Y-A Y-S1 Y-Q1 Y-01 Y-01-01 ... Y-01-31
 * Y-02 Y-02-01 ... Y-02-28 (Y-02-29) (Y-02-30) Y-02-31
 * ...
 * This means every year has exactly 391 = 12 * 32 + 4qrt + 2semi + 1full
 * Congruencies mod 32, 97, 195
 * years start at 1 */
static const int8_t yday_ad[] = {0,1,5,7,9,10,14,15,16,19,20,22};
static const int8_t qday_ad[] = {0,0,1,3};
/* q*90=qday_ad can be calced as 97q-yday_ad[3q+!!q] */

static inline FDate
_mkFDate(unsigned int y, unsigned int m, int d)
{
	/* fast forward feb padding */
	d = m != 2U || d < 29 + _leapp(y) ? d : 31;
	/* operate 0 based */
	y--, m--;
	return 3U + y * 391U + m * 32U + (m / 6U) + (m / 3U) + d;
}

static FDate
_rdFDate(const char *s)
{
	unsigned int y = 0U;
	unsigned int m = 0U;
	unsigned int d = 0U;

	for (; ((unsigned char)*s ^ '0') < 10U; s++) {
		y *= 10U;
		y += (unsigned char)*s ^ '0';
	}
	switch (*s++) {
	case '-':
	case '/':
		break;
	case ' ':
		break;
	default:
		goto nope;
	}
	switch (*s) {
	case 'A':
		d--;
	case 'S':
		d--;
	case 'Q':
		d--;
		s++;
		break;
	default:
		break;
	}
	for (; ((unsigned char)*s ^ '0') < 10U; s++) {
		m *= 10U;
		m += (unsigned char)*s ^ '0';
	}
	if (LIKELY(!d)) {
		switch (*s++) {
		case '\0':
			s--;
		case '-':
		case '/':
			break;
		default:
			goto nope;
		}
		for (; ((unsigned char)*s ^ '0') < 10U; s++) {
			d *= 10;
			d += (unsigned char)*s ^ '0';
		}
	} else if (m && d > -3U) {
		m--;
		m *= -d * 3;
		m++;
		m += !m;
	} else if (!m) {
		m = 1U;
	} else {
		goto nope;
	}
	if (*s || (m - 1U) >= 12U || (int)d >= 32) {
		goto nope;
	}
	return _mkFDate(y, m, d);
nope:
	return (FDate)-1;
}

static size_t
_prFDate(char *restrict buf, size_t bsz, FDate x)
{
	unsigned int y = x / 391U;
	unsigned int yd = x % 391U;
	unsigned int md = (yd + 192U) % 195U % 97U % 32U;
	unsigned int mo = (yd - md) / 32U;
	unsigned int qd = (yd % 97U - yd / 195U);
	size_t z = 4U;

	y++, mo++;
	buf[3U] = (y % 10U) ^ '0', y /= 10U;
	buf[2U] = (y % 10U) ^ '0', y /= 10U;
	buf[1U] = (y % 10U) ^ '0', y /= 10U;
	buf[0U] = (y % 10U) ^ '0';
	buf[4U] = '-';
	if (LIKELY(yd && md)) {
		buf[6U] = (mo % 10U) ^ '0', mo /= 10U;
		buf[5U] = (mo % 10U) ^ '0';
		buf[7U] = '-';
		buf[9U] = (md % 10U) ^ '0', md /= 10U;
		buf[8U] = (md % 10U) ^ '0';
		z = 10U;
	} else {
		static char qi[] = "ASQ0";
		buf[5U] = qi[qd % 4U];
		switch (qd % 4U) {
		case 0U:
			break;
		case 1U:
		case 2U:
			buf[6U] = (yd / (195U / qd) + 1) ^ '0';
			break;
		case 3U:
			buf[6U] = (mo % 10) ^ '0', mo /= 10U;
			buf[5U] = (mo % 10) ^ '0';
			break;
		}
		z = 7U - !qd;
	}
	buf[z] = '\0';
	return z;
}


SEXP
year_EDate(SEXP x)
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
yday_EDate(SEXP x)
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
month_EDate(SEXP x)
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
month_bang(SEXP x, SEXP value)
{
	/* Mar-based */
	static unsigned int moyd[] = {
		307U, 338U, 1U, 32U, 62U, 93U,
		123U, 154U, 185U, 215U, 246U, 276U, 307U,
	};
	R_xlen_t n = XLENGTH(x);
	SEXP ans;

	PROTECT(ans = allocVector(INTSXP, n));

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = INTEGER(x)[i];
		int m2b = INTEGER(value)[i] - 1;

		if (m != NA_INTEGER && (unsigned int)m2b < 12U) {
			unsigned int yyd = _yyd(m);
			unsigned int y = (yyd >> 16U);
			unsigned int yd = yyd & 0xffffU;
			unsigned int pend = (yd - 1) % 153U;
			unsigned int md = (2U * pend % 61U) / 2U;
			int yd2b;

			yd2b = moyd[m2b] + md;

			/* massage y into Jan years */
			y += yd >= 307U;
			y -= yd2b >= 307U;

			/* clamp to year's end */
			yd2b = yd2b <= 365 ? yd2b : !_leapp(y+1) ? 365 : 366;
			/* clamp to month's last */
			yd2b -= m2b > 3U && yd2b >= moyd[m2b + 1U];

			INTEGER(ans)[i] = _j00(y) + yd2b - 1;
		} else {
			INTEGER(ans)[i] = NA_INTEGER;
		}
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
mday_EDate(SEXP x)
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
mday_bang(SEXP x, SEXP value)
{
	/* Mar-based */
	static unsigned int mond[] = {
		31U, 30U, 31U, 30U, 31U,
		31U, 30U, 31U, 30U, 31U,
		31U, 29U,
	};
	R_xlen_t n = XLENGTH(x);
	SEXP ans;

	PROTECT(ans = allocVector(INTSXP, n));

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = INTEGER(x)[i];
		int md2b = INTEGER(value)[i] - 1;

		if (m != NA_INTEGER && (unsigned int)md2b <= 31U) {
			unsigned int yyd = _yyd(m);
			unsigned int y = (yyd >> 16U);
			unsigned int yd = yyd & 0xffffU;
			unsigned int pent = (yd - 1) / 153U;
			unsigned int pend = (yd - 1) % 153U;
			unsigned int mo = (2U * pend / 61U);
			unsigned int md = (2U * pend % 61U) / 2U;
			int yd2b;

			mo += 5U * pent;
			md2b = md2b < mond[mo] ? md2b : mond[mo] - 1;
			yd2b = yd - md + md2b;
			yd2b -= yd2b > 365 && !_leapp(y+1U);
			INTEGER(ans)[i] = _j00(y) + yd2b - 1;
		} else {
			INTEGER(ans)[i] = NA_INTEGER;
		}
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


SEXP
CFDate(SEXP y, SEXP m, SEXP d)
{
	R_xlen_t n = XLENGTH(y);
	SEXP ans;

	PROTECT(ans = allocVector(INTSXP, n));

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int xy = INTEGER(y)[i];
		int xm = INTEGER(m)[i];
		int xd = INTEGER(d)[i];
		INTEGER(ans)[i] = _mkFDate(xy, xm, xd);
	}

	UNPROTECT(1);
	return ans;
}

SEXP
as_FDate_character(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans;

	PROTECT(ans = allocVector(INTSXP, n));

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		SEXP s = STRING_ELT(x, i);
		FDate d;

		if (UNLIKELY(s == NA_STRING || !((d = _rdFDate(CHAR(s)))+1))) {
			INTEGER(ans)[i] = NA_INTEGER;
			continue;
		}

		INTEGER(ans)[i] = d;
	}

	with (SEXP class) {
		PROTECT(class = allocVector(STRSXP, 1));
		SET_STRING_ELT(class, 0, mkChar("FDate"));
		classgets(ans, class);
	}

	UNPROTECT(2);
	return ans;
}

SEXP
format_FDate(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans;

	PROTECT(ans = allocVector(STRSXP, n));
	/* no omp here as mkCharLen doesn't like it */
	for (R_xlen_t i = 0; i < n; i++) {
		int d = INTEGER(x)[i];
		char buf[32U];

		if (d != NA_INTEGER) {
			SET_STRING_ELT(ans, i, mkCharLen(buf, _prFDate(buf, sizeof(buf), d)));
		} else {
			SET_STRING_ELT(ans, i, NA_STRING);
		}
	}
	UNPROTECT(1);

	return ans;
}

SEXP
as_POSIXlt_FDate(SEXP x)
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
			unsigned int y = m / 391;
			unsigned int yd = m % 391;
			int md = (yd + 192) % 195 % 97 % 32;
			unsigned int mo = (yd - md) / 32;

			REAL(VECTOR_ELT(ans, 0))[i] = 0.;
			INTEGER(VECTOR_ELT(ans, 1))[i] = 0;
			INTEGER(VECTOR_ELT(ans, 2))[i] = 0;
			/* mday */
			INTEGER(VECTOR_ELT(ans, 3))[i] = md;
			/* mon */
			INTEGER(VECTOR_ELT(ans, 4))[i] = mo;
			/* year */
			INTEGER(VECTOR_ELT(ans, 5))[i] = y - 1899;
			/* wday */
			INTEGER(VECTOR_ELT(ans, 6))[i] = (m + 3) % 7;
			/* yday */
			INTEGER(VECTOR_ELT(ans, 7))[i] = 0;
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

SEXP
year_FDate(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans;

	PROTECT(ans = allocVector(INTSXP, n));

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = INTEGER(x)[i];
		unsigned int y = m / 391U;

		/* massage y and yd into Jan years */
		INTEGER(ans)[i] = m != NA_INTEGER ? y + 1 : NA_INTEGER;
	}

	UNPROTECT(1);
	return ans;
}

SEXP
yday_FDate(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans;

	PROTECT(ans = allocVector(INTSXP, n));

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = INTEGER(x)[i];
		unsigned int y = m / 391U;
		unsigned int yd = m % 391U;
		unsigned int md = (yd + 192U) % 195U % 97U % 32U;
		unsigned int mo = (yd - md) / 32U;

		/* massage y and yd into Jan years */
		INTEGER(ans)[i] = m != NA_INTEGER
			? yd && md
			? yd - 3 - yday_ad[mo] + (mo-2U<10U && _leapp(y+1U)) : 0
			: NA_INTEGER;
	}

	UNPROTECT(1);
	return ans;
}

SEXP
semi_FDate(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans;

	PROTECT(ans = allocVector(INTSXP, n));

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = INTEGER(x)[i];
		unsigned int yd = m % 391U;

		INTEGER(ans)[i] = m != NA_INTEGER
			? (yd > 0U) + (yd > 195U)
			: NA_INTEGER;
	}

	UNPROTECT(1);
	return ans;
}

SEXP
sday_FDate(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans;

	PROTECT(ans = allocVector(INTSXP, n));

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = INTEGER(x)[i];
		unsigned int y = m / 391U;
		unsigned int yd = m % 391U;
		unsigned int q = (yd - 2 - (yd > 195U)) / 97U;
		unsigned int md = (yd + 192U) % 195U % 97U % 32U;
		unsigned int mo = (yd - md) / 32U;
		/* q adjustments of yday are 0,90,91,92 = 0,90,181,273 */

		INTEGER(ans)[i] = m != NA_INTEGER
			? yd && md
			? yd - 3 - yday_ad[mo] - ((yd > 195U)*2U*90U + qday_ad[(yd > 195U)*2U]) + (mo-2U<4U && _leapp(y+1U))
			: 0
			: NA_INTEGER;
	}

	UNPROTECT(1);
	return ans;
}

SEXP
quarter_FDate(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans;

	PROTECT(ans = allocVector(INTSXP, n));

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = INTEGER(x)[i];
		unsigned int yd = m % 391U;
		unsigned int md = (yd + 192U) % 195U % 97U % 32U;
		unsigned int qd = (yd % 97U - (yd > 195U));
		unsigned int q = (yd + 95U - (yd > 195U)) / 97U;

		INTEGER(ans)[i] = m != NA_INTEGER
			? yd && md || qd%4U/2U ? q : 0
			: NA_INTEGER;
	}

	UNPROTECT(1);
	return ans;
}

SEXP
qday_FDate(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans;

	PROTECT(ans = allocVector(INTSXP, n));

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = INTEGER(x)[i];
		unsigned int y = m / 391U;
		unsigned int yd = m % 391U;
		unsigned int q = (yd - 2 - (yd > 195U)) / 97U;
		unsigned int md = (yd + 192U) % 195U % 97U % 32U;
		unsigned int mo = (yd - md) / 32U;

		INTEGER(ans)[i] = m != NA_INTEGER
			? yd && md
			? yd - 3 - yday_ad[mo] - (q*90U + qday_ad[q]) + (mo==2U && _leapp(y+1U))
			: 0
			: NA_INTEGER;
	}

	UNPROTECT(1);
	return ans;
}

SEXP
month_FDate(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans;

	PROTECT(ans = allocVector(INTSXP, n));

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = INTEGER(x)[i];
		unsigned int yd = m % 391U;
		unsigned int md = (yd + 192U) % 195U % 97U % 32U;
		unsigned int qd = (yd % 97U - (yd > 195U));
		unsigned int mo = (yd - md) / 32U;

		INTEGER(ans)[i] = m != NA_INTEGER
			? yd && md || !((qd+1U)%4U) ? mo + 1 : 0
			: NA_INTEGER;
	}

	UNPROTECT(1);
	return ans;
}

SEXP
mday_FDate(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans;

	PROTECT(ans = allocVector(INTSXP, n));

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = INTEGER(x)[i];
		unsigned int yd = m % 391U;
		unsigned int md = (yd + 192U) % 195U % 97U % 32U;

		INTEGER(ans)[i] = m != NA_INTEGER ? yd ? md : 0 : NA_INTEGER;
	}

	UNPROTECT(1);
	return ans;
}
