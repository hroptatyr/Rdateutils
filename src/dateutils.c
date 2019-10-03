#include <unistd.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <R.h>
#include <Rdefines.h>
#include "dateutils.h"
#include "nifty.h"

typedef union {
	struct {
		/* assume ieee floats */
		int d:30;
		int m:30;
	};
	uint64_t x;
} ddur;

#define DDURSXP		REALSXP
#define DDUR(x)		((ddur*)DATAPTR(x))
#define DDUR_ELT(x, i)	(DDUR(x)[i])
#define NA_DDUR		((ddur){.x = -1ULL})

static size_t
itostr(char *restrict buf, size_t bsz, int v)
{
	unsigned int u = v >= 0 ? v : -v;
	size_t z = 0U;

	*buf = '-';
	buf += v < 0, bsz -= v < 0;
	do {
		buf[z++] = (unsigned char)(u % 10U) ^ '0';
	} while (z < bsz && (u /= 10U));
	for (size_t i = 0U; i < z / 2U; i++) {
		char x = buf[i];
		buf[i] = buf[z - i - 1U];
		buf[z - i - 1U] = x;
	}
	buf[z] = '!';
	return z + (v < 0);
}

static inline int
cmp(unsigned int a, unsigned int b)
{
	/* calc cmp where -1 means A < B, 0 means A == B, and 1 means A > B */
	return (a < b) - (b < a);
}


/* Financial calendar
 * Y-A Y-S1 Y-Q1 Y-01 Y-01-01 ... Y-01-31
 * Y-02 Y-02-01 ... Y-02-28 (Y-02-29) (Y-02-30) Y-02-31
 * ...
 * This means every year has exactly 391 = 12 * 32 + 4qrt + 2semi + 1full
 * Congruencies mod 32, 97, 195
 * years start at 1 */
static const int_fast8_t yday_adj[] = {0,1,5,7,9,10,14,15,16,19,20,22,23};
static const int_fast8_t qday_adj[] = {0,0,1,3};
/* q*90=qday_ad can be calced as 97q-yday_ad[3q+!!q] */
static const int_fast16_t yday_eom[] = {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365};
static const int_fast16_t yday_Eom[] = {0, 31, 61, 92, 122, 153, 184, 214, 245, 275, 306, 337, 365};

static inline EDate
_j00(unsigned int y)
{
	return y * 365U + y / 4U - y / 100U + y / 400U;
}

static inline unsigned int
_year(EDate x)
{
	unsigned int guess = x / 365U;
	guess -= _j00(guess) >= x;
	guess -= _j00(guess) >= x;
	/* more corrections in the year 3000 or so */
	return guess;
}

static inline int
_leapp(unsigned int y)
{
	return !((y % 4U) || !(y % 100U) && (y % 400U));
}

static inline EDate
_mkEDate(unsigned int y, unsigned int m, int d)
{
	unsigned int yd;
	unsigned int eo;

	/* Mar is 0 */
	m += 9U/*==-3*/, m %= 12U;
	yd = yday_Eom[m + 0U] + d;
	eo = yday_Eom[m + 1U];
	eo += m==11U && _leapp(y);
	return _j00(y - (m >= 10U)) + (yd <= eo ? yd : eo);
}

static EDate
_rdEDate(const char *s)
{
	unsigned int y = 0U;
	unsigned int m = 0U;
	unsigned int d = 0U;
	char sep = '\0';

	for (; ((unsigned char)*s ^ '0') < 10U; s++) {
		y *= 10U;
		y += (unsigned char)*s ^ '0';
	}
	switch ((sep = *s++)) {
	case '-':
	case '/':
	case ' ':
		break;
	case '\0':
		/* could be 20190202 */
		m = y % 10000U;
		y /= 10000U;
		d = m % 100U;
		m /= 100U;
		s--;
		goto chk;
	default:
		goto nope;
	}
	for (; ((unsigned char)*s ^ '0') < 10U; s++) {
		m *= 10U;
		m += (unsigned char)*s ^ '0';
	}
	if (UNLIKELY(*s++ != sep)) {
		goto nope;
	}
	for (; ((unsigned char)*s ^ '0') < 10U; s++) {
		d *= 10;
		d += (unsigned char)*s ^ '0';
	}
chk:
	if (*s || (m - 1U) >= 12U || d >= 32U) {
		goto nope;
	}
	return _mkEDate(y, m, d);
nope:
	return (EDate)0U;
}

static EDate
_rdEDate_int(int x)
{
	unsigned int y, m, d;

	y = x / 10000;
	m = x % 10000;
	d = m % 100;
	m /= 100;
	if (UNLIKELY((m - 1U) >= 12U || d >= 32U)) {
		return (EDate)0U;
	}
	return _mkEDate(y, m, d);
}

static size_t
_prEDate(char *restrict buf, size_t bsz, EDate x)
{
	unsigned int y = _year(x);
	unsigned int yd = x - _j00(y) - 1;
	unsigned int pent = yd / 153U;
	unsigned int pend = yd % 153U;
	unsigned int mo = (2U * pend / 61U);
	unsigned int md = (2U * pend % 61U) / 2U;

	y += yd >= 306U;
	mo = (5U * pent + mo + 2U) % 12U + 1U;
	md++;

	buf[3U] = (y % 10U) ^ '0', y /= 10U;
	buf[2U] = (y % 10U) ^ '0', y /= 10U;
	buf[1U] = (y % 10U) ^ '0', y /= 10U;
	buf[0U] = (y % 10U) ^ '0';
	buf[4U] = '-';
	buf[6U] = (mo % 10U) ^ '0', mo /= 10U;
	buf[5U] = (mo % 10U) ^ '0';
	buf[7U] = '-';
	buf[9U] = (md % 10U) ^ '0', md /= 10U;
	buf[8U] = (md % 10U) ^ '0';
	buf[10U] = '\0';
	return 10U;
}

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
	char sep = '\0';

	for (; ((unsigned char)*s ^ '0') < 10U; s++) {
		y *= 10U;
		y += (unsigned char)*s ^ '0';
	}
	switch ((sep = *s++)) {
	case '-':
	case '/':
		break;
	case ' ':
		break;
	case '\0':
		if (y < 10000U) {
			return _mkFDate(y, 1, -3);
		}
		/* could be 20190202 */
		m = y % 10000U;
		y /= 10000U;
		d = m % 100U;
		m /= 100U;
		s--;
		goto chk;
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
		switch (*s) {
		case '\0':
			break;
		case '-':
		case '/':
			if (UNLIKELY(*s++ != sep)) {
				goto nope;
			}
			for (; ((unsigned char)*s ^ '0') < 10U; s++) {
				d *= 10;
				d += (unsigned char)*s ^ '0';
			}
			break;
		default:
			goto nope;
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
chk:
	if (*s || (m - 1U) >= 12U || (int)d >= 32) {
		goto nope;
	}
	return _mkFDate(y, m, d);
nope:
	return (FDate)-1;
}

static FDate
_rdFDate_int(int x)
{
	unsigned int y, m, d;

	y = x / 10000;
	m = x % 10000;
	d = m % 100;
	m /= 100;
	if (UNLIKELY((m - 1U) >= 12U || d >= 32U)) {
		return (FDate)-1;
	}
	return _mkFDate(y, m, d);
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

static ddur
_rdddur(const char *s)
{
	ddur r = {0, 0};
	int neg;
	int tmp;

	s += *s == 'P';
more:
	tmp = 0;
	s += neg = *s == '-';
	/* capture spots and junk*/
	switch (*s) {
	case 'S':
		tmp++;
	case 'T':
		tmp++;
	case 'O':
		tmp++;
		if (s[1U] != 'N' || s[2U] != '\0') {
			goto nope;
		}
		r.d = !neg ? tmp : -tmp;
		goto out;
	case '\0':
		goto out;
	default:
		break;
	}
	for (; ((unsigned char)*s ^ '0') < 10U; s++) {
		tmp *= 10;
		tmp += (unsigned char)*s ^ '0';
	}
	switch (*s++) {
	case 'Y':
		tmp *= 12;
	case 'M':
		r.m += !neg ? tmp : -tmp;
		goto more;
	case 'W':
		tmp *= 7;
	case 'D':
		r.d += !neg ? tmp : -tmp;
		goto more;
	case '\0':
		if (!r.m && !r.d) {
			break;
		}
	default:
		goto nope;
	}
out:
	return r;	
nope:
	return (ddur){NA_INTEGER, NA_INTEGER};
}

static size_t
_prddur(char *restrict buf, size_t bsz, const ddur d)
{
	int z = 0;

	buf[z++] = 'P';
	if (d.m) {
		z += itostr(buf + z, bsz - z, d.m);
		buf[z++] = 'M';
	}
	if (d.d) {
		z += itostr(buf + z, bsz - z, d.d);
		buf[z++] = 'D';
	}
	if (!d.m && !d.d) {
		buf[z++] = '0';
		buf[z++] = 'D';
	}
	buf[z] = '\0';
	return z;
}

static inline int
_is_na_ddur(ddur x)
{
	return !(x.x+1ULL);
}


SEXP
as_EDate_character(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const SEXP *xp = STRING_PTR_RO(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		SEXP s = xp[i];
		EDate d;

		if (UNLIKELY(s == NA_STRING || !(d = _rdEDate(CHAR(s))))) {
			ansp[i] = NA_INTEGER;
			continue;
		}

		ansp[i] = d;
	}

	with (SEXP class) {
		PROTECT(class = allocVector(STRSXP, 2));
		SET_STRING_ELT(class, 0, mkChar("EDate"));
		SET_STRING_ELT(class, 1, mkChar(".duo"));
		classgets(ans, class);
	}

	UNPROTECT(2);
	return ans;
}

SEXP
as_EDate_integer(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	int *xp = INTEGER(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = xp[i];
		EDate d;

		if (UNLIKELY(m == NA_INTEGER || !(d = _rdEDate_int(m)))) {
			ansp[i] = NA_INTEGER;
			continue;
		}

		ansp[i] = d;
	}

	with (SEXP class) {
		PROTECT(class = allocVector(STRSXP, 2));
		SET_STRING_ELT(class, 0, mkChar("EDate"));
		SET_STRING_ELT(class, 1, mkChar(".duo"));
		classgets(ans, class);
	}

	UNPROTECT(2);
	return ans;
}

SEXP
format_EDate(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(STRSXP, n));
	const int *xp = INTEGER(x);

	/* no omp here as mkCharLen doesn't like it */
	for (R_xlen_t i = 0; i < n; i++) {
		int d = xp[i];
		char buf[32U];

		if (d != NA_INTEGER) {
			SET_STRING_ELT(ans, i, mkCharLen(buf, _prEDate(buf, sizeof(buf), d)));
		} else {
			SET_STRING_ELT(ans, i, NA_STRING);
		}
	}
	UNPROTECT(1);

	return ans;
}

SEXP
as_FDate_EDate(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	int *xp = INTEGER(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = xp[i];

		if (LIKELY(m != NA_INTEGER)) {
			unsigned int y = _year(m);
			unsigned int yd = m - _j00(y) - 1;
			unsigned int pent = yd / 153U;
			unsigned int pend = yd % 153U;
			unsigned int mo = (2U * pend / 61U);
			unsigned int md = (2U * pend % 61U) / 2U;
			unsigned int mm = (5U * pent + mo + 2U) % 12U;

			ansp[i] = _mkFDate(y+(yd >= 306U), mm+1U, md+1U);
		} else {
			ansp[i] = NA_INTEGER;
		}
	}

	with (SEXP class) {
		PROTECT(class = allocVector(STRSXP, 2));
		SET_STRING_ELT(class, 0, mkChar("FDate"));
		SET_STRING_ELT(class, 1, mkChar(".duo"));
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
			unsigned int y = _year(m);
			unsigned int yd = m - _j00(y) - 1;
			unsigned int pent = yd / 153U;
			unsigned int pend = yd % 153U;
			unsigned int mo = (2U * pend / 61U);
			unsigned int md = (2U * pend % 61U) / 2U + 1U;
			unsigned int mm = (5U * pent + mo + 2U) % 12U;

			/* massage y and yd into Jan years */
			y += yd >= 306U;
			yd += yd < 306U ? 59U + _leapp(y) : -306U;

			ansp0U[i] = 0.;
			ansp[1U][i] = 0;
			ansp[2U][i] = 0;
			/* mday */
			ansp[3U][i] = md;
			/* mon */
			ansp[4U][i] = mm;
			/* year */
			ansp[5U][i] = y - 1900;
			/* wday */
			ansp[6U][i] = (m + 2U) % 7U;
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
year_EDate(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const int *xp = INTEGER(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = xp[i];
		unsigned int y = _year(m);
		unsigned int yd = m - _j00(y);

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
	const int *vp = INTEGER(value);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = xp[i];
		int y2b = vp[i];
		unsigned int y = _year(m);
		unsigned int yd = m - _j00(y);

		/* massage y and yd into Jan years */
		yd -= yd > 365U && !_leapp(y2b);
		y2b -= yd >= 307U;
		ansp[i] = m != NA_INTEGER ? _j00(y2b) + yd : NA_INTEGER;
	}

	with (SEXP class) {
		PROTECT(class = allocVector(STRSXP, 2));
		SET_STRING_ELT(class, 0, mkChar("EDate"));
		SET_STRING_ELT(class, 1, mkChar(".duo"));
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
		unsigned int y = _year(m);
		unsigned int yd = m - _j00(y);

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
	const int *vp = INTEGER(value);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = xp[i];
		int yd2b = vp[i];
		unsigned int y = _year(m);
		unsigned int yd = m - _j00(y);

		/* massage y into Jan years */
		y += yd >= 307U;
		/* clamp between -366/366 */
		yd2b = yd2b <= 365 ? yd2b : !_leapp(y) ? 365 : 366;
		yd2b = yd2b >= -364 ? yd2b : !_leapp(y-1) ? -364 : -365;
		yd2b -= 59 + _leapp(y);
		ansp[i] = m != NA_INTEGER ? _j00(y) + yd2b : NA_INTEGER;
	}

	with (SEXP class) {
		PROTECT(class = allocVector(STRSXP, 2));
		SET_STRING_ELT(class, 0, mkChar("EDate"));
		SET_STRING_ELT(class, 1, mkChar(".duo"));
		classgets(ans, class);
	}

	UNPROTECT(2);
	return ans;
}

SEXP
semi_EDate(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const int *xp = INTEGER(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = xp[i];
		unsigned int y = _year(m);
		unsigned int yd = m - _j00(y) - 1;
#if 0
		unsigned int pent = yd / 153U;
		unsigned int pend = yd % 153U;
		unsigned int mo = (2U * pend / 61U);
		unsigned int s = (5U * pent + mo + 2U) / 6U % 2U;
#else
		unsigned int s = 0U;

		s += yd >= yday_Eom[4U];
		s -= yd >= yday_Eom[10U];
#endif
		ansp[i] = m != NA_INTEGER ? s + 1U : NA_INTEGER;
	}

	UNPROTECT(1);
	return ans;
}

SEXP
sday_EDate(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const int *xp = INTEGER(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = xp[i];
		unsigned int y = _year(m);
		unsigned int yd = m - _j00(y);
		unsigned int sd;

		y += yd >= 307U;
		sd = yd <= yday_Eom[4U]
			? yd + 59U + _leapp(y)
			: yd <= yday_Eom[10U]
			? yd - yday_Eom[4U]
			: yd - yday_Eom[10U];
		ansp[i] = m != NA_INTEGER ? sd : NA_INTEGER;
	}

	UNPROTECT(1);
	return ans;
}

SEXP
quarter_EDate(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const int *xp = INTEGER(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = xp[i];
		unsigned int y = _year(m);
		unsigned int yd = m - _j00(y) - 1;
#if 0
		unsigned int pent = yd / 153U;
		unsigned int pend = yd % 153U;
		unsigned int mo = (2U * pend / 61U);
		unsigned int q = (5U * pent + mo + 2U) / 3U % 4U;
#else
		unsigned int q = 0U;

		q += yd >= yday_Eom[1U];
		q += yd >= yday_Eom[4U];
		q += yd >= yday_Eom[7U];
		q += yd >= yday_Eom[10U];
		q %= 4U;
#endif
		ansp[i] = m != NA_INTEGER ? q + 1U : NA_INTEGER;
	}

	UNPROTECT(1);
	return ans;
}

SEXP
qday_EDate(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const int *xp = INTEGER(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = xp[i];
		unsigned int y = _year(m);
		unsigned int yd = m - _j00(y);
		unsigned int qd;

		y += yd >= 307U;
		qd = yd <= yday_Eom[1U]
			? yd + 59U + _leapp(y)
			: yd <= yday_Eom[4U]
			? yd - yday_Eom[1U]
			: yd <= yday_Eom[7U]
			? yd - yday_Eom[4U]
			: yd <= yday_Eom[10U]
			? yd - yday_Eom[7U]
			: yd - yday_Eom[10U];
		ansp[i] = m != NA_INTEGER ? qd : NA_INTEGER;
	}

	UNPROTECT(1);
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
		unsigned int y = _year(m);
		unsigned int yd = m - _j00(y) - 1;
		unsigned int pent = yd / 153U;
		unsigned int pend = yd % 153U;
		unsigned int mo = (2U * pend / 61U);
		unsigned int mm = (5U * pent + mo + 2U) % 12U;

		ansp[i] = m != NA_INTEGER ? mm + 1U : NA_INTEGER;
	}

	UNPROTECT(1);
	return ans;
}

SEXP
month_bang(SEXP x, SEXP value)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const int *xp = INTEGER(x);
	const int *vp = INTEGER(value);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = xp[i];
		int m2b = vp[i] - 1;

		if (m != NA_INTEGER && (unsigned int)m2b < 12U) {
			unsigned int y = _year(m);
			unsigned int yd = m - _j00(y);
			unsigned int pend = (yd - 1) % 153U;
			unsigned int md = (2U * pend % 61U) / 2U;
			unsigned int eo;
			int yd2b;

			m2b += 10U;
			m2b %= 12U;
			yd2b = yday_Eom[m2b] + md + 1U;
			/* massage y into Jan years */
			y += yd >= 307U;
			y -= yd2b >= 307U;

			/* clamp to month's last */
			eo = yday_Eom[m2b + 1U];
			eo += m2b >= 11U && _leapp(y+1U);
			yd2b = yd2b <= eo ? yd2b : eo;

			ansp[i] = _j00(y) + yd2b;
		} else {
			ansp[i] = NA_INTEGER;
		}
	}

	with (SEXP class) {
		PROTECT(class = allocVector(STRSXP, 2));
		SET_STRING_ELT(class, 0, mkChar("EDate"));
		SET_STRING_ELT(class, 1, mkChar(".duo"));
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
		unsigned int y = _year(m);
		unsigned int yd = m - _j00(y) - 1;
		unsigned int pend = yd % 153U;
		unsigned int md = (2U * pend % 61U) / 2U;

		ansp[i] = m != NA_INTEGER ? md + 1U : NA_INTEGER;
	}

	UNPROTECT(1);
	return ans;
}

SEXP
mday_bang(SEXP x, SEXP value)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const int *xp = INTEGER(x);
	const int *vp = INTEGER(value);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = xp[i];
		int md2b = vp[i];

		if (m != NA_INTEGER && (unsigned int)md2b <= 31U) {
			unsigned int y = _year(m);
			unsigned int yd = m - _j00(y) - 1;
			unsigned int pent = yd / 153U;
			unsigned int pend = yd % 153U;
			unsigned int mo = (2U * pend / 61U);
			unsigned int md = (2U * pend % 61U) / 2U;
			unsigned int mz;
			int yd2b;

			mo += 5U * pent;
			mz = yday_Eom[mo + 1U] - yday_Eom[mo];
			mz += mo >= 11U && _leapp(y+1U);
			md2b = md2b <= mz ? md2b : mz;
			yd2b = yd - md + md2b;
			ansp[i] = _j00(y) + yd2b;
		} else {
			ansp[i] = NA_INTEGER;
		}
	}

	with (SEXP class) {
		PROTECT(class = allocVector(STRSXP, 2));
		SET_STRING_ELT(class, 0, mkChar("EDate"));
		SET_STRING_ELT(class, 1, mkChar(".duo"));
		classgets(ans, class);
	}

	UNPROTECT(2);
	return ans;
}

SEXP
week_EDate(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const int *xp = INTEGER(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		static const int_fast8_t iso[] = {2, 1, 0, -1, -2, 4, 3, 2};
		int m = xp[i];
		unsigned int y = _year(m);
		unsigned int yd = m - _j00(y) - 1;
		/* f01 is the wday of Jan-01 */
		unsigned int f01 = (y + y / 4U - y / 100U + y / 400U + 1U) % 7U;
		unsigned int w;

		y += yd >= 306U;
		yd += yd < 306U ? 59U : -305U;
		w = (7U + yd - iso[f01]) / 7;
		ansp[i] = m != NA_INTEGER ? w : NA_INTEGER;
	}

	UNPROTECT(1);
	return ans;
}

SEXP
wday_EDate(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const int *xp = INTEGER(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = xp[i];
		unsigned int wd = (m + 2U) % 7U;

		ansp[i] = m != NA_INTEGER ? wd : NA_INTEGER;
	}

	UNPROTECT(1);
	return ans;
}

SEXP
seq_EDate(SEXP from, SEXP till, SEXP by)
{
	EDate fd = INTEGER_ELT(from, 0U);
	EDate td = INTEGER_ELT(till, 0U);
	ddur d = DDUR_ELT(by, 0U);
	SEXP ans;
	unsigned int m = fd < td ? td - fd : fd - td;
	EDate *tmp = Calloc(m + 1U, EDate);
	size_t z = 0U;
	const int c = cmp(fd, td);

	if (cmp) {
		unsigned int negdd = d.d < 0 ? -d.d : 0;
		unsigned int posdd = d.d > 0 ? d.d : 0;
		EDate old = fd;

		do {
			tmp[z++] = fd;

			fd -= negdd;
			/* dm is assumed to be != 0 */
			if (d.m) {
				unsigned int y = _year(fd);
				unsigned int yd = fd - _j00(y) - 1;
				unsigned int pent = yd / 153U;
				unsigned int pend = yd % 153U;
				unsigned int mo = (2U * pend / 61U);
				unsigned int md = (2U * pend % 61U) / 2U;
				int mm = (5U * pent + mo) % 12U;
				unsigned int eo;

				y += (mm + d.m) / 12;
				mm = (mm + d.m) % 12;
				/* make sure residues are non-negative */
				y -= mm < 0;
				mm = (mm + 12) % 12;

				/* stay within month bounds */
				eo = yday_Eom[mm + 1U] + (mm >= 11U && _leapp(y+1U));
				yd = yday_Eom[mm] + md + 1U;
				yd = yd < eo ? yd : eo;

				fd = _j00(y) + yd;
			}
			fd += posdd;
			/* the exit condition is equivalent to
			 * old < fd < td  if the original fd < td  and
			 * old > fd > td  if the original fd > td
			 * equality is handled after this if-block */
		} while (cmp(fd, td) == c && cmp(old, fd) == c);
		/* make sure we deal with empty sums */
		z -= cmp(fd, old) == c;
	}
	/* seq is inclusive, so finalise tmp if they're equal */
	tmp[z] = fd;
	z += fd == td;

	/* now for real */
	ans = PROTECT(allocVector(INTSXP, z));
	memcpy(INTEGER(ans), tmp, z * sizeof(*tmp));
	Free(tmp);

	with (SEXP class = PROTECT(allocVector(STRSXP, 2))) {
		SET_STRING_ELT(class, 0, mkChar("EDate"));
		SET_STRING_ELT(class, 1, mkChar(".duo"));
		classgets(ans, class);
	}

	UNPROTECT(2);
	return ans;
}

/* EDate arith */
SEXP
plus_EDate(SEXP x, SEXP y)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const int *xp = INTEGER(x);
	const ddur *yp = DDUR(y);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = xp[i];
		ddur d = yp[i];

		if (UNLIKELY(m == NA_INTEGER || _is_na_ddur(d))) {
			ansp[i] = NA_INTEGER;
			continue;
		}
		if (!d.m || d.d < 0) {
			/* negative day periods take precedence
			 * this is to make subtraction somewhat inverse to addition:
			 * X + PiMjD - PiMjD = X + PiMjD + P-iM-jD
			 * = X + PiM + PjD + P-jD + P-iM  = X */
			m += d.d;
		}
		if (d.m) {
			unsigned int y = _year(m);
			unsigned int yd = m - _j00(y) - 1;
			unsigned int pent = yd / 153U;
			unsigned int pend = yd % 153U;
			unsigned int mo = (2U * pend / 61U);
			unsigned int md = (2U * pend % 61U) / 2U;
			int mm = (5U * pent + mo) % 12U;
			unsigned int eo;

			y += (mm + d.m) / 12;
			mm = (mm + d.m) % 12;
			/* make sure residues are non-negative */
			y -= mm < 0;
			mm = (mm + 12) % 12;

			/* stay within month bounds */
			eo = yday_Eom[mm + 1U] + (mm >= 11U && _leapp(y+1U));
			yd = yday_Eom[mm] + md + 1U;
			yd = yd < eo ? yd : eo;

			m = _j00(y) + yd + (d.d > 0 ? d.d : 0);
		}
		ansp[i] = m;
	}

	with (SEXP class) {
		PROTECT(class = allocVector(STRSXP, 2));
		SET_STRING_ELT(class, 0, mkChar("EDate"));
		SET_STRING_ELT(class, 1, mkChar(".duo"));
		classgets(ans, class);
	}

	UNPROTECT(2);
	return ans;
}

SEXP
minus_EDate(SEXP x, SEXP y)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(DDURSXP, n));
	ddur *restrict ansp = DDUR(ans);
	const int *xp = INTEGER(x);
	const int *yp = INTEGER(y);

	/* no omp here as mkCharLen doesn't like it */
	for (R_xlen_t i = 0; i < n; i++) {
		int u = xp[i];
		int v = yp[i];

		if (u != NA_INTEGER && v != NA_INTEGER) {
			unsigned int uy = _year(u);
			unsigned int uyd = u - _j00(uy) - 1;
			unsigned int upent = uyd / 153U;
			unsigned int upend = uyd % 153U;
			unsigned int umo = (2U * upend / 61U);
			unsigned int umd = (2U * upend % 61U) / 2U;
			unsigned int umm = 5U * upent + umo;
			unsigned int vy = _year(v);
			unsigned int vyd = v - _j00(vy) - 1;
			unsigned int vpent = vyd / 153U;
			unsigned int vpend = vyd % 153U;
			unsigned int vmo = (2U * vpend / 61U);
			unsigned int vmd = (2U * vpend % 61U) / 2U;
			unsigned int vmm = 5U * vpent + vmo;

			ansp[i] = (ddur){umd - vmd, (uy - vy) * 12 + (umm - vmm)};
		} else {
			ansp[i] = NA_DDUR;
		}
	}

	with (SEXP class) {
		PROTECT(class = allocVector(STRSXP, 2));
		SET_STRING_ELT(class, 0, mkChar("ddur"));
		SET_STRING_ELT(class, 1, mkChar(".duo"));
		classgets(ans, class);
	}

	UNPROTECT(2);
	return ans;
}

SEXP
ddur_EDate(SEXP x, SEXP y)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(DDURSXP, n));
	ddur *restrict ansp = DDUR(ans);
	const int *xp = INTEGER(x);
	const int *yp = INTEGER(y);

	/* no omp here as mkCharLen doesn't like it */
	for (R_xlen_t i = 0; i < n; i++) {
		int u = xp[i];
		int v = yp[i];
		ddur d = {v - u};

		ansp[i] = u != NA_INTEGER && v != NA_INTEGER ? d : NA_DDUR;
	}

	with (SEXP class) {
		PROTECT(class = allocVector(STRSXP, 2));
		SET_STRING_ELT(class, 0, mkChar("ddur"));
		SET_STRING_ELT(class, 1, mkChar(".duo"));
		classgets(ans, class);
	}

	UNPROTECT(2);
	return ans;
}


SEXP
as_FDate_character(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const SEXP *xp = STRING_PTR_RO(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		SEXP s = xp[i];
		FDate d;

		if (UNLIKELY(s == NA_STRING || !((d = _rdFDate(CHAR(s)))+1))) {
			ansp[i] = NA_INTEGER;
			continue;
		}

		ansp[i] = d;
	}

	with (SEXP class) {
		PROTECT(class = allocVector(STRSXP, 2));
		SET_STRING_ELT(class, 0, mkChar("FDate"));
		SET_STRING_ELT(class, 1, mkChar(".duo"));
		classgets(ans, class);
	}

	UNPROTECT(2);
	return ans;
}

SEXP
as_FDate_integer(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	int *xp = INTEGER(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = xp[i];
		FDate d;

		if (UNLIKELY(m == NA_INTEGER || !((d = _rdFDate_int(m))+1))) {
			ansp[i] = NA_INTEGER;
			continue;
		}

		ansp[i] = d;
	}

	with (SEXP class) {
		PROTECT(class = allocVector(STRSXP, 2));
		SET_STRING_ELT(class, 0, mkChar("FDate"));
		SET_STRING_ELT(class, 1, mkChar(".duo"));
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
as_EDate_FDate(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	const int *xp = INTEGER(x);
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = xp[i];

		if (LIKELY(m != NA_INTEGER)) {
			unsigned int y = m / 391;
			unsigned int yd = m % 391;
			unsigned int md = (yd + 192U) % 195U % 97U % 32U;
			unsigned int mo = (yd - md) / 32;

			/* round to EDate boundaries */
			md += !md;
			md &= !yd - !!yd;
			mo &= -!!yd;
			ansp[i] = _mkEDate(y+1U, mo+1U, md);
		} else {
			ansp[i] = NA_INTEGER;
		}
	}

	with (SEXP class) {
		PROTECT(class = allocVector(STRSXP, 2));
		SET_STRING_ELT(class, 0, mkChar("EDate"));
		SET_STRING_ELT(class, 1, mkChar(".duo"));
		classgets(ans, class);
	}

	UNPROTECT(2);
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

			md += mo>1U && _leapp(y+1U);
			eo += mo>0U && _leapp(y+1U);

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

		ansp[i] = m != NA_INTEGER && (yd > 0U)
			? 1U + (yd > 195U)
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
		unsigned int sd;

		if (m != NA_INTEGER && yd && md) {
			unsigned int eo = yday_eom[mo + 1U] - yday_eom[6U*(yd>195U)];

			md += mo>1U && _leapp(y+1U) && yd<=195U;
			eo += mo>0U && _leapp(y+1U) && yd<=195U;

			yd = yday_eom[mo] + md - yday_eom[6U*(yd>195U)];
			sd = yd <= eo ? yd : eo;
		} else if (m != NA_INTEGER && qd%4U == 1U) {
			sd = 0;
		} else {
			sd = NA_INTEGER;
		}
		ansp[i] = sd;
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
			? yd && md || qd%4U/2U ? q : NA_INTEGER
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

			md += mo>1U && _leapp(y+1U) && !q;
			eo += mo>0U && _leapp(y+1U) && !q;

			yd = yday_eom[mo] + md - yday_eom[3U*q];
			qd = yd <= eo ? yd : eo;
		} else if (m != NA_INTEGER && qd%4U == 2U) {
			qd = 0;
		} else {
			qd = NA_INTEGER;
		}
		ansp[i] = qd;
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
			? yd && md || !((qd+1U)%4U) ? mo + 1 : NA_INTEGER
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
			md = md <= eo ? md : eo;
		} else if (m != NA_INTEGER && qd%4U == 3U) {
			md = 0;
		} else {
			md = NA_INTEGER;
		}
		ansp[i] = md;
	}

	UNPROTECT(1);
	return ans;
}

SEXP
week_FDate(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const int *xp = INTEGER(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		static const int_fast8_t iso[] = {2, 1, 0, -1, -2, 4, 3, 2};
		int m = xp[i];
		unsigned int y = m / 391U;
		unsigned int yd = m % 391U;
		unsigned int md = (yd + 192U) % 195U % 97U % 32U;
		unsigned int mo = (yd - md) / 32U;

		if (m != NA_INTEGER && yd && md) {
			unsigned int eo = yday_eom[mo + 1U];
			/* f01 is the wday of Jan-01 */
			unsigned int f01 = (y + y / 4U - y / 100U + y / 400U + 1U) % 7U;

			md += mo>2U && _leapp(y+1U);
			eo += mo>1U && _leapp(y+1U);

			yd = yday_eom[mo] + md;
			yd = yd <= eo ? yd : eo;
			yd = (7 + yd - iso[f01]) / 7;
		} else {
			yd = NA_INTEGER;
		}
		ansp[i] = yd;
	}

	UNPROTECT(1);
	return ans;
}

SEXP
wday_FDate(SEXP x)
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
		unsigned int wd;

		if (m != NA_INTEGER && yd && md) {
			unsigned int eo = yday_eom[mo + 1U];
			/* f00 is the wday of Jan-00 */
			unsigned int f00 = y + y / 4U - y / 100U + y / 400U;

			md += mo>1U && _leapp(y+1U);
			eo += mo>0U && _leapp(y+1U);

			yd = yday_eom[mo] + md;
			yd = yd <= eo ? yd : eo;

			wd = (f00 + yd) % 7U;
		} else {
			wd = NA_INTEGER;
		}
		ansp[i] = wd;
	}

	UNPROTECT(1);
	return ans;
}

SEXP
seq_FDate(SEXP from, SEXP till, SEXP by)
{
	FDate fd = INTEGER_ELT(from, 0U);
	FDate td = INTEGER_ELT(till, 0U);
	ddur d = DDUR_ELT(by, 0U);
	SEXP ans;
	FDate *tmp;
	size_t z = 0U;
	const int c = cmp(fd, td);
	/* decomp */
	FDate old = fd;
	unsigned int y = fd / 391U;
	unsigned int yd = fd % 391U;
	int md = (yd + 192U) % 195U % 97U % 32U;
	int mo = (yd - md) / 32U;

	if (UNLIKELY(!(yd && md) && d.d)) {
	nope:
		error("duration incompatible with start or end date");
	} else if (!(yd && md)) {
		/* special dates, only allowed for d.d == 0 */
		unsigned int qd = (yd % 97U - (yd > 195U));

		switch (qd%4U) {
		case 0U:
			if (d.m % 12) {
				goto nope;
			} else if (!d.m) {
				d.m = 12;
			}
			break;
		case 1U:
			if (d.m % 6) {
				goto nope;
			} else if (!d.m) {
				d.m = 6;
			}
			break;
		case 2U:
			if (d.m % 3) {
				goto nope;
			} else if (!d.m) {
				d.m = 3;
			}
			break;
		case 3U:
			d.m += !d.m;
			break;
		}

		/* we use at least month steps */
		with (unsigned int m = fd < td ? td - fd : fd - td) {
			m >>= 5U;
			tmp = Calloc(m + 1U, FDate);
		}
		if (cmp) {
			do {
				/* what we've got in the last round */
				tmp[z++] = fd;

				switch (qd%4U) {
				case 0U:
					mo = 0, md = -3;
					break;
				case 1U:
					md = -2;
					break;
				case 2U:
					md = -1;
					break;
				case 3U:
					break;
				}
				y += (mo + d.m) / 12;
				mo = (mo + d.m) % 12;
				/* make sure residues are non-negative */
				y -= mo < 0;
				mo = (mo + 12) % 12;

				/* calculate next date */
				fd = _mkFDate(y+1U, mo+1U, md);

				/* redo the decomp */
				y = fd / 391U;
				yd = fd % 391U;
				md = (yd + 192U) % 195U % 97U % 32U;
				mo = (yd - md) / 32U;
				qd = (yd % 97U - (yd > 195U));

				/* the exit condition is equivalent to
				 * old < fd < td  if the original fd < td  and
				 * old > fd > td  if the original fd > td
				 * equality is handled after this if-block */
			} while (cmp(fd, td) == c && cmp(old, fd) == c);
			/* make sure we deal with empty sums */
			z -= cmp(fd, old) == c;
		}
	} else if (cmp) {
		with (unsigned int m = fd < td ? td - fd : fd - td) {
			/* if only month steps, save on allocation */
			m >>= !d.d * 5U;
			tmp = Calloc(m + 1U, FDate);
		}
		do {
			/* what we've got in the last round */
			tmp[z++] = fd;

			if (d.d < 0) {
				/* negative day periods take precedence
				 * this is to make subtraction somewhat inverse to addition:
				 * X + PiMjD - PiMjD = X + PiMjD + P-iM-jD
				 * = X + PiM + PjD + P-jD + P-iM  = X */
				unsigned int eo;

				/* stay within month bounds */
				eo = yday_eom[mo + 1U] - yday_eom[mo];
				eo += mo==1U && _leapp(y+1U);
				md = md <= eo ? md : eo;

				md += d.d;
				while (md <= 0) {
					y -= !mo;
					mo = mo > 0U ? mo - 1U : 11U;

					eo = yday_eom[mo + 1U] - yday_eom[mo];
					eo += mo==1U && _leapp(y+1U);
					md += eo;
				}
			}
			if (d.m) {
				y += (mo + d.m) / 12;
				mo = (mo + d.m) % 12;
				/* make sure residues are non-negative */
				y -= mo < 0;
				mo = (mo + 12) % 12;
			}
			if (d.d > 0) {
				unsigned int eo;

				/* stay within month bounds */
				eo = yday_eom[mo + 1U] - yday_eom[mo];
				eo += mo==1U && _leapp(y+1U);
				md = md <= eo ? md : eo;

				md += d.d;
				while (md > eo) {
					md -= eo;
					mo++;
					y += mo >= 12U;
					mo = mo < 12U ? mo : 0U;

					eo = yday_eom[mo + 1U] - yday_eom[mo];
					eo += mo==1U && _leapp(y+1U);
				}
			}

			/* next candidate */
			fd = _mkFDate(y+1U, mo+1U, md);

			/* redo the decomp */
			y = fd / 391U;
			yd = fd % 391U;
			md = (yd + 192U) % 195U % 97U % 32U;
			mo = (yd - md) / 32U;

			/* the exit condition is equivalent to
			 * old < fd < td  if the original fd < td  and
			 * old > fd > td  if the original fd > td
			 * equality is handled after this if-block */
		} while (cmp(fd, td) == c && cmp(old, fd) == c);
		/* make sure we deal with empty sums */
		z -= cmp(fd, old) == c;
	} else {
		tmp = Calloc(1U, FDate);
	}
	/* seq is inclusive, so finalise tmp if they're equal */
	tmp[z] = fd;
	z += fd == td;

	/* now for real */
	ans = PROTECT(allocVector(INTSXP, z));
	memcpy(INTEGER(ans), tmp, z * sizeof(*tmp));
	Free(tmp);

	with (SEXP class = PROTECT(allocVector(STRSXP, 2))) {
		SET_STRING_ELT(class, 0, mkChar("FDate"));
		SET_STRING_ELT(class, 1, mkChar(".duo"));
		classgets(ans, class);
	}

	UNPROTECT(2);
	return ans;
}

/* date arith */
SEXP
plus_FDate(SEXP x, SEXP y)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const int *xp = INTEGER(x);
	const ddur *yp = DDUR(y);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		unsigned int m = xp[i];
		ddur d = yp[i];
		unsigned int y = m / 391U;
		unsigned int yd = m % 391U;
		int md = (yd + 192U) % 195U % 97U % 32U;
		int mo = (yd - md) / 32U;

		if (UNLIKELY(m == NA_INTEGER || _is_na_ddur(d) || !(yd && md) && d.d)) {
		nope:
			ansp[i] = NA_INTEGER;
			continue;
		} else if (!(yd && md)) {
			unsigned int qd = (yd % 97U - (yd > 195U));

			switch (qd%4U) {
			case 0U:
				if (d.m % 12) {
					goto nope;
				}
				mo = 0, md = -3;
				break;
			case 1U:
				if (d.m % 6) {
					goto nope;
				}
				md = -2;
				break;
			case 2U:
				if (d.m % 3) {
					goto nope;
				}
				md = -1;
				break;
			case 3U:
				break;
			}
			y += (mo + d.m) / 12;
			mo = (mo + d.m) % 12;
			/* make sure residues are non-negative */
			y -= mo < 0;
			mo = (mo + 12) % 12;
			goto out;
		}
		if (d.d < 0) {
			/* negative day periods take precedence
			 * this is to make subtraction somewhat inverse to addition:
			 * X + PiMjD - PiMjD = X + PiMjD + P-iM-jD
			 * = X + PiM + PjD + P-jD + P-iM  = X */
			unsigned int eo;

			/* stay within month bounds */
			eo = yday_eom[mo + 1U] - yday_eom[mo];
			eo += mo==1U && _leapp(y+1U);
			md = md <= eo ? md : eo;

			md += d.d;
			while (md <= 0) {
				y -= !mo;
				mo = mo > 0U ? mo - 1U : 11U;

				eo = yday_eom[mo + 1U] - yday_eom[mo];
				eo += mo==1U && _leapp(y+1U);
				md += eo;
			}
		}
		if (d.m) {
			y += (mo + d.m) / 12;
			mo = (mo + d.m) % 12;
			/* make sure residues are non-negative */
			y -= mo < 0;
			mo = (mo + 12) % 12;
		}
		if (d.d > 0) {
			unsigned int eo;

			/* stay within month bounds */
			eo = yday_eom[mo + 1U] - yday_eom[mo];
			eo += mo==1U && _leapp(y+1U);
			md = md <= eo ? md : eo;

			md += d.d;
			while (md > eo) {
				md -= eo;
				mo++;
				y += mo >= 12U;
				mo = mo < 12U ? mo : 0U;

				eo = yday_eom[mo + 1U] - yday_eom[mo];
				eo += mo==1U && _leapp(y+1U);
			}
		}
	out:
		ansp[i] = _mkFDate(y+1U, mo+1U, md);
	}

	with (SEXP class) {
		PROTECT(class = allocVector(STRSXP, 2));
		SET_STRING_ELT(class, 0, mkChar("FDate"));
		SET_STRING_ELT(class, 1, mkChar(".duo"));
		classgets(ans, class);
	}

	UNPROTECT(2);
	return ans;
}

SEXP
minus_FDate(SEXP x, SEXP y)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(DDURSXP, n));
	ddur *restrict ansp = DDUR(ans);
	const int *xp = INTEGER(x);
	const int *yp = INTEGER(y);

	/* no omp here as mkCharLen doesn't like it */
	for (R_xlen_t i = 0; i < n; i++) {
		int u = xp[i];
		int v = yp[i];
		ddur d;

		if (LIKELY(u != NA_INTEGER && v != NA_INTEGER)) {
			unsigned int uy = u / 391U;
			unsigned int uyd = u % 391U;
			int umd = (uyd + 192U) % 195U % 97U % 32U;
			int umo = (uyd - umd) / 32U;
			unsigned int vy = v / 391U;
			unsigned int vyd = v % 391U;
			int vmd = (vyd + 192U) % 195U % 97U % 32U;
			int vmo = (vyd - vmd) / 32U;

			umo &= -!!uyd;
			vmo &= -!!vyd;
			umd &= -!!uyd;
			vmd &= -!!vyd;
			/* upgrade to ordinary date when at least 1 is ordinary */
			umd += !umd && !!vmd;
			vmd += !vmd && !!umd;
			d.m = 12 * (uy - vy) + (umo - vmo);
			d.d = umd - vmd;
			ansp[i] = d;
		} else {
			ansp[i] = NA_DDUR;
		}
	}

	with (SEXP class) {
		PROTECT(class = allocVector(STRSXP, 2));
		SET_STRING_ELT(class, 0, mkChar("ddur"));
		SET_STRING_ELT(class, 1, mkChar(".duo"));
		classgets(ans, class);
	}

	UNPROTECT(2);
	return ans;
}

SEXP
ddur_FDate(SEXP x, SEXP y)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(DDURSXP, n));
	ddur *restrict ansp = DDUR(ans);
	const int *xp = INTEGER(x);
	const int *yp = INTEGER(y);

	/* no omp here as mkCharLen doesn't like it */
	for (R_xlen_t i = 0; i < n; i++) {
		int u = xp[i];
		int v = yp[i];

		if (LIKELY(u != NA_INTEGER && v != NA_INTEGER)) {
			unsigned int uy = u / 391U;
			unsigned int uyd = u % 391U;
			int umd = (uyd + 192U) % 195U % 97U % 32U;
			int umo = (uyd - umd) / 32U;
			unsigned int vy = v / 391U;
			unsigned int vyd = v % 391U;
			int vmd = (vyd + 192U) % 195U % 97U % 32U;
			int vmo = (vyd - vmd) / 32U;
			EDate ud, vd;

			umo &= -!!uyd;
			vmo &= -!!vyd;
			umd &= -!!uyd;
			vmd &= -!!vyd;
			/* upgrade to ordinary date when at least 1 is ordinary */
			umd += !umd && !!vmd;
			vmd += !vmd && !!umd;
			ud = _mkEDate(uy+1U, umo+1U, umd);
			vd = _mkEDate(vy+1U, vmo+1U, vmd);
			ansp[i] = (ddur){vd - ud};
		} else {
			ansp[i] = NA_DDUR;
		}
	}

	with (SEXP class) {
		PROTECT(class = allocVector(STRSXP, 2));
		SET_STRING_ELT(class, 0, mkChar("ddur"));
		SET_STRING_ELT(class, 1, mkChar(".duo"));
		classgets(ans, class);
	}

	UNPROTECT(2);
	return ans;
}


SEXP
as_ddur_character(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(DDURSXP, n));
	ddur *restrict ansp = DDUR(ans);
	const SEXP *xp = STRING_PTR_RO(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		SEXP s = xp[i];
		ddur d;

		if (UNLIKELY(s == NA_STRING ||
			     (d = _rdddur(CHAR(s))).d == NA_INTEGER)) {
			ansp[i] = NA_DDUR;
			continue;
		}

		ansp[i] = d;
	}

	with (SEXP class) {
		PROTECT(class = allocVector(STRSXP, 2));
		SET_STRING_ELT(class, 0, mkChar("ddur"));
		SET_STRING_ELT(class, 1, mkChar(".duo"));
		classgets(ans, class);
	}

	UNPROTECT(2);
	return ans;
}

SEXP
as_ddur_numeric(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(DDURSXP, n));
	ddur *restrict ansp = DDUR(ans);

	switch (TYPEOF(x)) {
	case REALSXP: {
		const double *xp = REAL(x);

		#pragma omp parallel for
		for (R_xlen_t i = 0; i < n; i++) {
			double v = xp[i];

			ansp[i] = !R_IsNA(v) ? (ddur){(int)v} : NA_DDUR;
		}
		break;
	}
	case INTSXP: {
		const int *xp = INTEGER(x);

		#pragma omp parallel for
		for (R_xlen_t i = 0; i < n; i++) {
			int v = xp[i];

			ansp[i] = v != NA_INTEGER ? (ddur){v} : NA_DDUR;
		}
		break;
	}
	default:
		#pragma omp parallel for
		for (R_xlen_t i = 0; i < n; i++) {
			ansp[i] = NA_DDUR;
		}
		break;
	}

	with (SEXP class) {
		PROTECT(class = allocVector(STRSXP, 2));
		SET_STRING_ELT(class, 0, mkChar("ddur"));
		SET_STRING_ELT(class, 1, mkChar(".duo"));
		classgets(ans, class);
	}

	UNPROTECT(2);
	return ans;
}

SEXP
is_na_ddur(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(LGLSXP, n));
	int *restrict ansp = LOGICAL(ans);
	const ddur *xp = DDUR(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		ansp[i] = _is_na_ddur(xp[i]);
	}

	UNPROTECT(1);
	return ans;
}

SEXP
format_ddur(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(STRSXP, n));
	const ddur *xp = DDUR(x);

	/* no omp here as mkCharLen doesn't like it */
	for (R_xlen_t i = 0; i < n; i++) {
		char buf[64U];

		if (!_is_na_ddur(xp[i])) {
			SET_STRING_ELT(ans, i, mkCharLen(buf, _prddur(buf, sizeof(buf), xp[i])));
		} else {
			SET_STRING_ELT(ans, i, NA_STRING);
		}
	}
	UNPROTECT(1);
	return ans;
}

SEXP
year_ddur(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const ddur *xp = DDUR(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		ansp[i] = !_is_na_ddur(xp[i]) ? xp[i].m / 12 : NA_INTEGER;
	}

	UNPROTECT(1);
	return ans;
}

SEXP
semi_ddur(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const ddur *xp = DDUR(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		ansp[i] = !_is_na_ddur(xp[i]) ? xp[i].m / 6 : NA_INTEGER;
	}

	UNPROTECT(1);
	return ans;
}

SEXP
quarter_ddur(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const ddur *xp = DDUR(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		ansp[i] = !_is_na_ddur(xp[i]) ? xp[i].m / 3 : NA_INTEGER;
	}

	UNPROTECT(1);
	return ans;
}

SEXP
month_ddur(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const ddur *xp = DDUR(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		ansp[i] = !_is_na_ddur(xp[i]) ? xp[i].m : NA_INTEGER;
	}

	UNPROTECT(1);
	return ans;
}

SEXP
week_ddur(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const ddur *xp = DDUR(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		ansp[i] = !_is_na_ddur(xp[i]) ? xp[i].d / 7 : NA_INTEGER;
	}

	UNPROTECT(1);
	return ans;
}

SEXP
dday_ddur(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const ddur *xp = DDUR(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		ansp[i] = !_is_na_ddur(xp[i]) ? xp[i].d : NA_INTEGER;
	}

	UNPROTECT(1);
	return ans;
}

SEXP
plus_ddur(SEXP x, SEXP y)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(DDURSXP, n));
	ddur *restrict ansp = DDUR(ans);
	const ddur *xp = DDUR(x);
	const ddur *yp = DDUR(y);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		ddur dx = xp[i];
		ddur dy = yp[i];

		ansp[i] = !_is_na_ddur(xp[i]) && !_is_na_ddur(yp[i])
			? (ddur){dx.d+dy.d, dx.m+dy.m}
			: NA_DDUR;
	}

	with (SEXP class) {
		PROTECT(class = allocVector(STRSXP, 2));
		SET_STRING_ELT(class, 0, mkChar("ddur"));
		SET_STRING_ELT(class, 1, mkChar(".duo"));
		classgets(ans, class);
	}

	UNPROTECT(2);
	return ans;
}

SEXP
neg_ddur(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(DDURSXP, n));
	ddur *restrict ansp = DDUR(ans);
	const ddur *xp = DDUR(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		ddur d = xp[i];
		d.m = -d.m;
		d.d = -d.d;
		ansp[i] = d;
	}

	with (SEXP class) {
		PROTECT(class = allocVector(STRSXP, 2));
		SET_STRING_ELT(class, 0, mkChar("ddur"));
		SET_STRING_ELT(class, 1, mkChar(".duo"));
		classgets(ans, class);
	}

	UNPROTECT(2);
	return ans;
}

SEXP
mul_ddur(SEXP x, SEXP y)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(DDURSXP, n));
	ddur *restrict ansp = DDUR(ans);
	const ddur *xp = DDUR(x);
	const double *yp = REAL(y);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		ddur d = xp[i];
		d.m = (int)((double)d.m * yp[i]);
		d.d = (int)((double)d.d * yp[i]);
		ansp[i] = d;
	}

	with (SEXP class) {
		PROTECT(class = allocVector(STRSXP, 2));
		SET_STRING_ELT(class, 0, mkChar("ddur"));
		SET_STRING_ELT(class, 1, mkChar(".duo"));
		classgets(ans, class);
	}

	UNPROTECT(2);
	return ans;
}

SEXP
div_ddur(SEXP x, SEXP y)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(DDURSXP, n));
	ddur *restrict ansp = DDUR(ans);
	const ddur *xp = DDUR(x);
	const double *yp = REAL(y);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		ddur d = xp[i];
		d.m = (int)((double)d.m / yp[i]);
		d.d = (int)((double)d.d / yp[i]);
		ansp[i] = d;
	}

	with (SEXP class) {
		PROTECT(class = allocVector(STRSXP, 2));
		SET_STRING_ELT(class, 0, mkChar("ddur"));
		SET_STRING_ELT(class, 1, mkChar(".duo"));
		classgets(ans, class);
	}

	UNPROTECT(2);
	return ans;
}

SEXP
mod_ddur(SEXP x, SEXP y)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(DDURSXP, n));
	ddur *restrict ansp = DDUR(ans);
	const ddur *xp = DDUR(x);
	const double *yp = REAL(y);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		ddur d = xp[i];
		d.m = (int)(fmod((double)d.m, yp[i]));
		d.d = (int)(fmod((double)d.d, yp[i]));
		ansp[i] = d;
	}

	with (SEXP class) {
		PROTECT(class = allocVector(STRSXP, 2));
		SET_STRING_ELT(class, 0, mkChar("ddur"));
		SET_STRING_ELT(class, 1, mkChar(".duo"));
		classgets(ans, class);
	}

	UNPROTECT(2);
	return ans;
}
