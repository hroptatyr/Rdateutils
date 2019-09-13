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
static const int8_t yday_ad[] = {0,1,5,7,9,10,14,15,16,19,20,22,23};
static const int8_t qday_ad[] = {0,0,1,3};
/* q*90=qday_ad can be calced as 97q-yday_ad[3q+!!q] */
static const int16_t yday_eom[] = {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365};

static inline FDate
_mkFDate(unsigned int y, unsigned int m, int d)
{
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
	unsigned int qd = (yd % 97U - (yd > 195U));
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
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const int *xp = INTEGER(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = xp[i];
		unsigned int yyd = _yyd(m);
		unsigned int y = (yyd >> 16U);
		unsigned int yd = yyd & 0xffffU;

		/* massage y and yd into Jan years */
		y += yd >= 307U;
		ansp[i] = m != NA_INTEGER ? y : NA_INTEGER;
	}

	UNPROTECT(1);
	return ans;
}

SEXP
year_bang(SEXP x, SEXP value)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const int *xp = INTEGER(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = xp[i];
		int y2b = INTEGER(value)[i];
		unsigned int yyd = _yyd(m);
		unsigned int y = (yyd >> 16U);
		unsigned int yd = yyd & 0xffffU;

		/* massage y and yd into Jan years */
		yd -= yd > 365U && !_leapp(y2b);
		y2b -= yd >= 307U;
		ansp[i] = m != NA_INTEGER ? _j00(y2b) + yd - 1 : NA_INTEGER;
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
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const int *xp = INTEGER(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = xp[i];
		unsigned int yyd = _yyd(m);
		unsigned int y = (yyd >> 16U);
		unsigned int yd = yyd & 0xffffU;

		/* massage y and yd into Jan years */
		y += yd >= 307U;
		yd += yd < 307U ? 59U + _leapp(y) : -306U;
		ansp[i] = m != NA_INTEGER ? yd : NA_INTEGER;
	}

	UNPROTECT(1);
	return ans;
}

SEXP
yday_bang(SEXP x, SEXP value)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const int *xp = INTEGER(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = xp[i];
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
		ansp[i] = m != NA_INTEGER ? _j00(y) + yd2b - 1 : NA_INTEGER;
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
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const int *xp = INTEGER(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = xp[i];
		ansp[i] = m != NA_INTEGER ? _month(m) : NA_INTEGER;
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
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const int *xp = INTEGER(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = xp[i];
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

			ansp[i] = _j00(y) + yd2b - 1;
		} else {
			ansp[i] = NA_INTEGER;
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
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const int *xp = INTEGER(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = xp[i];
		ansp[i] = m != NA_INTEGER ? _mday(m) : NA_INTEGER;
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
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const int *xp = INTEGER(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = xp[i];
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
			ansp[i] = _j00(y) + yd2b - 1;
		} else {
			ansp[i] = NA_INTEGER;
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
	const int *xp = INTEGER(x);
	double *restrict ansp0U;
	int *restrict ansp[9U];
	SEXP ans;

	PROTECT(ans = allocVector(VECSXP, 9));
	SET_VECTOR_ELT(ans, 0, allocVector(REALSXP, n));
	ansp0U = REAL(VECTOR_ELT(ans, 0));
	for (int i = 1; i < 9; i++) {
		SET_VECTOR_ELT(ans, i, allocVector(INTSXP, n));
		ansp[i] = INTEGER(VECTOR_ELT(ans, i));
	}

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = xp[i];

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

			ansp0U[i] = 0.;
			ansp[1U][i] = 0;
			ansp[2U][i] = 0;
			/* mday */
			ansp[3U][i] = md;
			/* mon */
			ansp[4U][i] = (mo + 1U) % 12U;
			/* year */
			ansp[5U][i] = y - 1900;
			/* wday */
			ansp[6U][i] = (m + 3) % 7;
			/* yday */
			ansp[7U][i] = yd;
			/* is dst */
			ansp[8U][i] = -1;
		} else {
			ansp0U[i] = NA_REAL;
			ansp[1U][i] = NA_INTEGER;
			ansp[2U][i] = NA_INTEGER;
			/* mday */
			ansp[3U][i] = NA_INTEGER;
			/* mon */
			ansp[4U][i] = NA_INTEGER;
			/* year */
			ansp[5U][i] = NA_INTEGER;
			/* wday */
			ansp[6U][i] = NA_INTEGER;
			/* yday */
			ansp[7U][i] = NA_INTEGER;
			/* is dst */
			ansp[8U][i] = -1;
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
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const int *yp = INTEGER(y);
	const int *mp = INTEGER(m);
	const int *dp = INTEGER(d);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int xy = yp[i];
		int xm = mp[i];
		int xd = dp[i];
		ansp[i] = _mkFDate(xy, xm, xd);
	}

	UNPROTECT(1);
	return ans;
}

SEXP
as_FDate_character(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		SEXP s = STRING_ELT(x, i);
		FDate d;

		if (UNLIKELY(s == NA_STRING || !((d = _rdFDate(CHAR(s)))+1))) {
			ansp[i] = NA_INTEGER;
			continue;
		}

		ansp[i] = d;
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
	SEXP ans = PROTECT(allocVector(STRSXP, n));
	const int *xp = INTEGER(x);

	/* no omp here as mkCharLen doesn't like it */
	for (R_xlen_t i = 0; i < n; i++) {
		int d = xp[i];
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
	const int *xp = INTEGER(x);
	double *restrict ansp0U;
	int *restrict ansp[9U];
	SEXP ans;

	PROTECT(ans = allocVector(VECSXP, 9U));
	SET_VECTOR_ELT(ans, 0, allocVector(REALSXP, n));
	ansp0U = REAL(VECTOR_ELT(ans, 0));
	for (int i = 1; i < 9; i++) {
		SET_VECTOR_ELT(ans, i, allocVector(INTSXP, n));
		ansp[i] = INTEGER(VECTOR_ELT(ans, i));
	}

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = xp[i];
		unsigned int y = m / 391;
		unsigned int yd = m % 391;
		unsigned int md = (yd + 192U) % 195U % 97U % 32U;
		unsigned int mo = (yd - md) / 32;

		if (m != NA_INTEGER && yd && md) {
			unsigned int eo;

			/* embed yd */
			eo = yday_eom[mo + 1U];
			eo += mo>1U && _leapp(y+1U);
			yd = yday_eom[mo] + md + (mo>2U && _leapp(y+1U));
			yd = yd <= eo ? yd : eo;

			/* embed md */
			eo = yday_eom[mo + 1U] - yday_eom[mo];
			eo += mo==1U && _leapp(y+1U);
			md = md <= eo ? md : eo;

			ansp0U[i] = 0.;
			ansp[1U][i] = 0;
			ansp[2U][i] = 0;
			/* mday */
			ansp[3U][i] = md;
			/* mon */
			ansp[4U][i] = mo;
			/* year */
			ansp[5U][i] = y - 1899;
			/* wday */
			ansp[6U][i] = (m + 3) % 7;
			/* yday */
			ansp[7U][i] = yd;
			/* is dst */
			ansp[8U][i] = -1;
		} else {
			ansp0U[i] = NA_REAL;
			ansp[1U][i] = NA_INTEGER;
			ansp[2U][i] = NA_INTEGER;
			/* mday */
			ansp[3U][i] = NA_INTEGER;
			/* mon */
			ansp[4U][i] = NA_INTEGER;
			/* year */
			ansp[5U][i] = NA_INTEGER;
			/* wday */
			ansp[6U][i] = NA_INTEGER;
			/* yday */
			ansp[7U][i] = NA_INTEGER;
			/* is dst */
			ansp[8U][i] = -1;
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
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const int *xp = INTEGER(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = xp[i];
		unsigned int y = m / 391U;

		ansp[i] = m != NA_INTEGER ? y + 1 : NA_INTEGER;
	}

	UNPROTECT(1);
	return ans;
}

SEXP
yday_FDate(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const int *xp = INTEGER(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = xp[i];
		unsigned int y = m / 391U;
		unsigned int yd = m % 391U;
		unsigned int md = (yd + 192U) % 195U % 97U % 32U;
		unsigned int mo = (yd - md) / 32U;

		if (m != NA_INTEGER && yd && md) {
			unsigned int eo = yday_eom[mo + 1U];

			md += mo>2U && _leapp(y+1U);
			eo += mo>1U && _leapp(y+1U);

			yd = yday_eom[mo] + md;
			yd = yd <= eo ? yd : eo;
		} else if (m != NA_INTEGER && !yd) {
			yd = 0;
		} else {
			yd = NA_INTEGER;
		}
		ansp[i] = yd;
	}

	UNPROTECT(1);
	return ans;
}

SEXP
semi_FDate(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const int *xp = INTEGER(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = xp[i];
		unsigned int yd = m % 391U;

		ansp[i] = m != NA_INTEGER
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
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const int *xp = INTEGER(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = xp[i];
		unsigned int y = m / 391U;
		unsigned int yd = m % 391U;
		unsigned int md = (yd + 192U) % 195U % 97U % 32U;
		unsigned int mo = (yd - md) / 32U;
		unsigned int qd = (yd % 97U - (yd > 195U));

		if (m != NA_INTEGER && yd && md) {
			unsigned int eo = yday_eom[mo + 1U] - yday_eom[6U*(yd>195U)];

			md += mo>2U && _leapp(y+1U) && yd<=195U;
			eo += mo>1U && _leapp(y+1U) && yd<=195U;

			yd = yday_eom[mo] + md - yday_eom[6U*(yd>195U)];
			yd = yd <= eo ? yd : eo;
		} else if (m != NA_INTEGER && qd%4U == 1U) {
			yd = 0;
		} else {
			yd = NA_INTEGER;
		}
		ansp[i] = yd;
	}

	UNPROTECT(1);
	return ans;
}

SEXP
quarter_FDate(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const int *xp = INTEGER(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = xp[i];
		unsigned int yd = m % 391U;
		unsigned int md = (yd + 192U) % 195U % 97U % 32U;
		unsigned int qd = (yd % 97U - (yd > 195U));
		unsigned int q = (yd + 95U - (yd > 195U)) / 97U;

		ansp[i] = m != NA_INTEGER
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
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const int *xp = INTEGER(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = xp[i];
		unsigned int y = m / 391U;
		unsigned int yd = m % 391U;
		unsigned int md = (yd + 192U) % 195U % 97U % 32U;
		unsigned int mo = (yd - md) / 32U;
		unsigned int qd = (yd % 97U - (yd > 195U));
		unsigned int q = (yd - 2 - (yd > 195U)) / 97U;

		if (m != NA_INTEGER && yd && md) {
			unsigned int eo = yday_eom[mo + 1U] - yday_eom[3U*q];

			md += mo>2U && _leapp(y+1U) && !q;
			eo += mo>1U && _leapp(y+1U) && !q;

			yd = yday_eom[mo] + md - yday_eom[3U*q];
			yd = yd <= eo ? yd : eo;
		} else if (m != NA_INTEGER && qd%4U == 2U) {
			yd = 0;
		} else {
			yd = NA_INTEGER;
		}
		ansp[i] = yd;
	}

	UNPROTECT(1);
	return ans;
}

SEXP
month_FDate(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const int *xp = INTEGER(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = xp[i];
		unsigned int yd = m % 391U;
		unsigned int md = (yd + 192U) % 195U % 97U % 32U;
		unsigned int qd = (yd % 97U - (yd > 195U));
		unsigned int mo = (yd - md) / 32U;

		ansp[i] = m != NA_INTEGER
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
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const int *xp = INTEGER(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = xp[i];
		unsigned int y = m / 391U;
		unsigned int yd = m % 391U;
		unsigned int md = (yd + 192U) % 195U % 97U % 32U;
		unsigned int mo = (yd - md) / 32U;
		unsigned int qd = (yd % 97U - (yd > 195U));

		if (m != NA_INTEGER && yd && md) {
			unsigned int eo = yday_eom[mo + 1U] - yday_eom[mo];

			eo += mo==1U && _leapp(y+1U);
			yd = md <= eo ? md : eo;
		} else if (m != NA_INTEGER && qd%4U == 3U) {
			yd = 0;
		} else {
			yd = NA_INTEGER;
		}
		ansp[i] = yd;
	}

	UNPROTECT(1);
	return ans;
}
