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

typedef unsigned int EDate;
typedef unsigned int FDate;
typedef int wcnt;

#define DDURSXP		REALSXP
#define DDUR(x)		((ddur*)DATAPTR(x))
#define DDUR_ELT(x, i)	(DDUR(x)[i])
#define NA_DDUR		((ddur){.x = -1ULL})

#define WCNTSXP		INTSXP
#define WCNT(x)		((wcnt*)INTEGER(x))
#define WCNT_ELT(x, i)	(INTEGER(x)[i])
#define NA_WCNT		NA_INTEGER

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
cmp_ui(unsigned int a, unsigned int b)
{
	/* calc cmp where -1 means A < B, 0 means A == B, and 1 means A > B */
	return (a < b) - (b < a);
}

static inline int
cmp_ddur(ddur a, ddur b)
{
	/* calc cmp where -1 means A < B, 0 means A == B, and 1 means A > B */
	return (a.m < b.m || a.m == b.m && a.d < b.d) - (b.m < a.m || b.m == a.m && b.d < a.d);
}

static inline int
iabs(int x)
{
	return x >= 0 ? x : -x;
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
		d = -3U;
		m = 1U;
		s++;
		goto chk;
	case 'S':
		d = -2U;
		s++;
		break;
	case 'Q':
		if (s[1U]) {
			d = -1U;
			s++;
			break;
		}
		goto Q;
	case 'Z':
		m++;
	case 'X':
		m++;
	case 'V':
		m++;
	case 'U':
		m++;
	Q:
		/* is used twice, for august and quartal */
		m++;
	case 'N':
		m++;
	case 'M':
		m++;
	case 'K':
		m++;
	case 'J':
		m++;
	case 'H':
		m++;
	case 'G':
		m++;
	case 'F':
		m++;
		s++;
		goto chk;
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
	} else {
		goto nope;
	}
chk:
	if (*s || (unsigned int)(m - 1U) >= 12U || (int)d >= 32) {
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
	if (m) {
		d = m % 100;
		m /= 100;
		if (UNLIKELY((m - 1U) >= 12U || d >= 32U)) {
			return (FDate)-1;
		}
	} else {
		m = 1U;
		d = -3U;
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
	int leapp = _leapp(y+1);
	size_t z = 4U;

	y++, mo++;
	buf[3U] = (y % 10U) ^ '0', y /= 10U;
	buf[2U] = (y % 10U) ^ '0', y /= 10U;
	buf[1U] = (y % 10U) ^ '0', y /= 10U;
	buf[0U] = (y % 10U) ^ '0';
	buf[4U] = '-';
	if (LIKELY(yd && md)) {
		unsigned int eo;

		eo = yday_eom[mo] - yday_eom[mo - 1U] + (mo==2U && leapp);
		md = md <= eo ? md : eo;

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
		goto nope;
	default:
		break;
	}
	for (; ((unsigned char)*s ^ '0') < 10U; s++) {
		tmp *= 10;
		tmp += (unsigned char)*s ^ '0';
	}
	switch (*s++) {
	case 'Y':
	case 'y':
		tmp *= 12;
	case 'M':
	case 'm':
		r.m += !neg ? tmp : -tmp;
		goto maybe_more;
	case 'W':
	case 'w':
		tmp *= 7;
	case 'D':
	case 'd':
		r.d += !neg ? tmp : -tmp;
		goto maybe_more;
	case '\0':
		if (!r.m && !r.d) {
			break;
		}
	default:
		goto nope;
	maybe_more:
		if (!*s) {
			goto out;
		}
		goto more;
	}
out:
	return r;
nope:
	return NA_DDUR;
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

static wcnt
_rdwcnt(const char *s)
{
	unsigned int w = 0U;
	int c = 0;
	int neg;

	s += neg = *s == '-';
	for (; ((unsigned char)*s ^ '0') < 10U; s++) {
		c *= 10;
		c += (unsigned char)*s ^ '0';
	}
	if (!s[0U] || !s[1U]) {
		/* capture abbrev'd weekday */
		switch (*s) {
		case 'S':
			w++;
		case 'A':
			w++;
		case 'F':
			w++;
		case 'R':
			w++;
		case 'W':
			w++;
		case 'T':
			w++;
		case 'M':
			w++;
		case '\0':
			break;
		default:
			goto nope;
		}
	} else {
		/* 3 letter abbrev? */
		switch (*s) {
		case 'M':
			if (s[1U] != 'O' && s[1U] != 'o' ||
			    s[2U] != 'N' && s[2U] != 'n') {
				goto nope;
			}
			w = 1U;
			break;
		case 'T':
			if ((s[1U] == 'H' || s[1U] == 'h') &&
			    (s[2U] == 'U' || s[2U] == 'u')) {
				w = 4U;
			} else if ((s[1U] == 'U' || s[1U] == 'u') &&
				   (s[2U] == 'E' || s[2U] == 'e')) {
				w = 2U;
			} else {
				goto nope;
			}
			break;
		case 'W':
			if (s[1U] != 'E' && s[1U] != 'e' ||
			    s[2U] != 'D' && s[2U] != 'd') {
				goto nope;
			}
			w = 3U;
			break;
		case 'F':
			if (s[1U] != 'R' && s[1U] != 'r' ||
			    s[2U] != 'I' && s[2U] != 'i') {
				goto nope;
			}
			w = 5U;
			break;
		case 'S':
			if ((s[1U] == 'U' || s[1U] == 'u') &&
			    (s[2U] == 'N' || s[2U] == 'n')) {
				w = 7U;
			} else if ((s[1U] == 'A' || s[1U] == 'a') &&
				   (s[2U] == 'T' || s[2U] == 't')) {
				w = 6U;
			} else {
				goto nope;
			}
			break;
		default:
			goto nope;
		}
	}
	c += !c && neg;
	c = !neg ? c : -c;
	return c << 3 ^ w;
nope:
	return NA_INTEGER;
}

static size_t
_prwcnt(char *restrict buf, size_t bsz, const wcnt d)
{
	static const char W[] = "\0MTWRFAS";
	size_t z = 0U;
	int c = d >> 3;
	unsigned int w = d & 7U;

	if (c) {
		z += itostr(buf + z, bsz - z, c);
	}
	buf[z] = W[w];
	z += !!w;
	buf[z] = '\0';
	return z;
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
as_FDate_factor(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const int *xp = INTEGER(x);
	const SEXP *lvl;
	R_len_t nlvl;

	with (SEXP levels = getAttrib(x, R_LevelsSymbol)) {
		if (isNull(levels)) {
			error("Factor vector with no levels");
		}
		lvl = STRING_PTR_RO(levels);
		nlvl = length(levels);
	}

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int l = xp[i];
		FDate d;

		if (UNLIKELY(l == NA_INTEGER || (unsigned int)--l >= nlvl ||
			     !((d = _rdFDate(CHAR(lvl[l])))+1))) {
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
	const int *xp = INTEGER(x);

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
as_FDate_IDate(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const int *xp = INTEGER(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = xp[i] + 719469/*0000-03-00*/;

		if (LIKELY(xp[i] != NA_INTEGER)) {
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
as_IDate_FDate(SEXP x)
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
			ansp[i] = _mkEDate(y+1U, mo+1U, md) - 719469/*0000-03-00*/;
		} else {
			ansp[i] = NA_INTEGER;
		}
	}

	with (SEXP class) {
		PROTECT(class = allocVector(STRSXP, 2));
		SET_STRING_ELT(class, 0, mkChar("IDate"));
		SET_STRING_ELT(class, 1, mkChar("Date"));
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
		unsigned int w;

		if (m != NA_INTEGER && yd && md) {
			unsigned int eo = yday_eom[mo + 1U];
			/* f01 is the wday of Jan-01 */
			unsigned int f01 = (y + y / 4U - y / 100U + y / 400U + 1U) % 7U;

			md += mo>2U && _leapp(y+1U);
			eo += mo>1U && _leapp(y+1U);

			yd = yday_eom[mo] + md;
			yd = yd <= eo ? yd : eo;
			w = (7 + yd - iso[f01]) / 7;
		} else {
			w = NA_INTEGER;
		}
		ansp[i] = w;
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

		if (LIKELY(m != NA_INTEGER && yd && md)) {
			unsigned int eo = yday_eom[mo + 1U];
			/* f00 is the wday of Jan-00 */
			unsigned int f00 = y + y / 4U - y / 100U + y / 400U;

			md += mo>1U && _leapp(y+1U);
			eo += mo>0U && _leapp(y+1U);

			yd = yday_eom[mo] + md;
			yd = yd <= eo ? yd : eo;

			wd = (f00 + yd - 1U) % 7U + 1U;
		} else {
			wd = NA_INTEGER;
		}
		ansp[i] = wd;
	}

	UNPROTECT(1);
	return ans;
}

SEXP
sweek_FDate(SEXP x)
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
		unsigned int w;

		if (LIKELY(m != NA_INTEGER && yd && md)) {
			unsigned int eo = yday_eom[mo + 1U];
			/* f01 is the wday of Jan-01 */
			unsigned int f01 = (y + y / 4U - y / 100U + y / 400U + 1U) % 7U;
			int s4;

			md += mo>2U && _leapp(y+1U);
			eo += mo>1U && _leapp(y+1U);

			yd = yday_eom[mo] + md;
			yd = yd <= eo ? yd : eo;

			s4 = yday_eom[(mo >= 6U) * 6U] - 3;
			w = (7 + yd - iso[f01]) / 7 - (7 + s4 - iso[f01]) / 7;
		} else {
			w = NA_INTEGER;
		}
		ansp[i] = w;
	}

	UNPROTECT(1);
	return ans;
}

SEXP
qweek_FDate(SEXP x)
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
		unsigned int w;

		if (LIKELY(m != NA_INTEGER && yd && md)) {
			unsigned int eo = yday_eom[mo + 1U];
			/* f01 is the wday of Jan-01 */
			unsigned int f01 = (y + y / 4U - y / 100U + y / 400U + 1U) % 7U;
			int q4;

			md += mo>2U && _leapp(y+1U);
			eo += mo>1U && _leapp(y+1U);

			yd = yday_eom[mo] + md;
			yd = yd <= eo ? yd : eo;

			q4 = yday_eom[(mo / 3U) * 3U] - 3;
			w = (7 + yd - iso[f01]) / 7 - (7 + q4 - iso[f01]) / 7;
		} else {
			w = NA_INTEGER;
		}
		ansp[i] = w;
	}

	UNPROTECT(1);
	return ans;
}

SEXP
mweek_FDate(SEXP x)
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
		unsigned int w;

		if (LIKELY(m != NA_INTEGER && yd && md)) {
			unsigned int eo = yday_eom[mo + 1U];
			/* f01 is the wday of Jan-01 */
			unsigned int f01 = (y + y / 4U - y / 100U + y / 400U + 1U) % 7U;
			int m4;

			md += mo>2U && _leapp(y+1U);
			eo += mo>1U && _leapp(y+1U);

			yd = yday_eom[mo] + md;
			yd = yd <= eo ? yd : eo;

			m4 = yday_eom[mo] - 3;
			w = (7 + yd - iso[f01]) / 7 - (7 + m4 - iso[f01]) / 7;
		} else {
			w = NA_INTEGER;
		}
		ansp[i] = w;
	}

	UNPROTECT(1);
	return ans;
}

SEXP
wcnt_FDate(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(WCNTSXP, n));
	int *restrict ansp = WCNT(ans);
	const int *xp = INTEGER(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = xp[i];
		unsigned int y = m / 391U;
		unsigned int yd = m % 391U;
		unsigned int md = (yd + 192U) % 195U % 97U % 32U;
		unsigned int mo = (yd - md) / 32U;

		if (LIKELY(m != NA_INTEGER && yd && md)) {
			unsigned int eo = yday_eom[mo + 1U];
			/* f00 is the wday of Jan-00 */
			unsigned int f00 = y + y / 4U - y / 100U + y / 400U;
			unsigned int wc;
			unsigned int wd;

			md += mo>1U && _leapp(y+1U);
			eo += mo>0U && _leapp(y+1U);

			yd = yday_eom[mo] + md;
			yd = yd <= eo ? yd : eo;

			wc = (yd - 1U) / 7U + 1U;
			wd = (f00 + yd - 1U) % 7U + 1U;
			ansp[i] = wc << 3 ^ wd;
		} else {
			ansp[i] = NA_INTEGER;
		}
	}

	with (SEXP class) {
		PROTECT(class = allocVector(STRSXP, 2));
		SET_STRING_ELT(class, 0, mkChar("wcnt"));
		SET_STRING_ELT(class, 1, mkChar(".duo"));
		classgets(ans, class);
	}

	UNPROTECT(2);
	return ans;
}

SEXP
mwcnt_FDate(SEXP x)
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
		unsigned int wc;

		if (m != NA_INTEGER && yd && md) {
			unsigned int eo = yday_eom[mo + 1U] - yday_eom[mo];
			/* f00 is the wday of Jan-00 */
			unsigned int f00 = y + y / 4U - y / 100U + y / 400U;
			unsigned int wc;
			unsigned int wd;

			eo += mo==1U && _leapp(y+1U);
			md = md <= eo ? md : eo;
			yd = yday_eom[mo] + md;
			wc = (md - 1U) / 7U + 1U;
			wd = (f00 + yd - 1U) % 7U + 1U;
			ansp[i] = wc << 3 ^ wd;
		} else {
			ansp[i] = NA_INTEGER;
		}
	}

	with (SEXP class) {
		PROTECT(class = allocVector(STRSXP, 2));
		SET_STRING_ELT(class, 0, mkChar("wcnt"));
		SET_STRING_ELT(class, 1, mkChar(".duo"));
		classgets(ans, class);
	}

	UNPROTECT(2);
	return ans;
}

SEXP
year_bang_FDate(SEXP x, SEXP value)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const int *xp = INTEGER(x);
	const int *vp = INTEGER(value);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = xp[i];
		int y2b = vp[i] - 1;
		unsigned int yd = m % 391U;

		ansp[i] = m != NA_INTEGER && (unsigned int)y2b < 10000U
			? y2b * 391 + yd : NA_INTEGER;
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
yday_bang_FDate(SEXP x, SEXP value)
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
		unsigned int y = m / 391U;
		int ly = _leapp(y+1);
		int mo;
		int md;

		yd2b = yd2b < (365 + ly) ? yd2b : 365 + ly;
		yd2b = yd2b >= 0 ? yd2b : (365 + ly) + (yd2b + 1);
		yd2b = yd2b >= 0 ? yd2b : 0;
		yd2b--;

#if 0
		if ((yd2b -= ly) < 59) {
			/* third trimester */
			yd2b += ly;
		} else if ((yd2b += 2) < 153 + 61) {
			/* first trimester */
			;
		} else {
			/* second trimester */
			yd2b += 30;
		}
#else
/* branchless version */
		/* first and second trimester */
		yd2b -= ly;
		/* second trimester */
		yd2b += (yd2b >= 151 + 61) << 5U;
		/* third and first trimester */
		yd2b += (yd2b >= 59 && yd2b < 151 + 61) << 1U;
		yd2b += ly && yd2b < 59;
#endif

		mo = 2 * yd2b / 61;
		md = 2 * yd2b % 61;

		md >>= md >= 0;
		md <<= md < 0;
		ansp[i] = m != NA_INTEGER ? _mkFDate(y+1, mo+1 - (mo>=7), md+1) : NA_INTEGER;
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
sday_bang_FDate(SEXP x, SEXP value)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const int *xp = INTEGER(x);
	const int *vp = INTEGER(value);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = xp[i];
		int sd2b = vp[i];

		if (LIKELY(m != NA_INTEGER)) {
			/* use yday semantics instead of iterating like in qday<- */
			unsigned int y = m / 391U;
			unsigned int yd = m % 391U;
			unsigned int s = yd > 195U;
			unsigned int ly = _leapp(y+1) && !s;
			int bo = yday_eom[6U*(s+0U)];
			int eo = yday_eom[6U*(s+1U)];
			int mo;
			int md;

			sd2b += sd2b >= 0 ? bo : eo + 1 + ly;
			sd2b = sd2b <= eo + ly ? sd2b : eo + ly;
			sd2b = sd2b >= bo ? sd2b : bo;
			sd2b--;

			/* first and second trimester */
			sd2b -= ly;
			/* second trimester */
			sd2b += (sd2b >= 151 + 61) << 5;
			/* third and first trimester */
			sd2b += (sd2b >= 59 && sd2b < 151 + 61) << 1;
			sd2b += ly && sd2b < 59;

			mo = 2 * sd2b / 61;
			md = 2 * sd2b % 61;

			md >>= md >= 0;
			md += md >= 0;

			mo += (sd2b - s <= bo);
			md -= (sd2b - s <= bo) << 5;

			ansp[i] = _mkFDate(y+1, mo+1 - (mo>=7), md);
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
qday_bang_FDate(SEXP x, SEXP value)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const int *xp = INTEGER(x);
	const int *vp = INTEGER(value);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = xp[i];
		int qd2b = vp[i];

		if (LIKELY(m != NA_INTEGER)) {
			unsigned int y = m / 391U;
			unsigned int yd = m % 391U;
			unsigned int md = (yd + 192U) % 195U % 97U % 32U;
			unsigned int mo = (yd - md) / 32U & -!!yd;
			unsigned int q = mo / 3U;
			unsigned int ly = _leapp(y+1);
			int bo = yday_eom[3U*(q+0U)];
			int eo = yday_eom[3U*(q+1U)];
			int ymo = 3 * q;
			int ymd;

			/* count from the end of the quarter if negative */
			qd2b += qd2b >= 0 ? bo : eo + 1 + (!q && ly);
			ymo += qd2b > yday_eom[ymo + 1];
			qd2b -= !q && ly;
			ymo += qd2b > yday_eom[ymo + 1];
			qd2b += !q && ly && qd2b < 60;
			qd2b = qd2b <= eo ? qd2b : eo;
			qd2b = qd2b >= bo ? qd2b : bo;
			ymd = qd2b - yday_eom[ymo];
			ymd -= !ymd;

			ansp[i] = _mkFDate(y+1, ymo+1, ymd);
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
month_bang_FDate(SEXP x, SEXP value)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const int *xp = INTEGER(x);
	const int *vp = INTEGER(value);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = xp[i];
		int m2b = vp[i];
		unsigned int y = m / 391U;
		unsigned int yd = m % 391U;
		unsigned int md = (yd + 192U) % 195U % 97U % 32U;

		ansp[i] = m != NA_INTEGER && (unsigned int)(m2b - 1) < 12U
			? _mkFDate(y+1, m2b, md) : NA_INTEGER;
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
mday_bang_FDate(SEXP x, SEXP value)
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
		unsigned int y = m / 391U;
		unsigned int yd = m % 391U;
		unsigned int md = (yd + 192U) % 195U % 97U % 32U;
		unsigned int mo = (yd - md) / 32U;

		mo &= -!!yd;
		md2b = md2b < 32 ? md2b : 31;
		md2b = md2b >= 0 ? md2b : yday_eom[mo+1] - yday_eom[mo] + (md2b + 1) + (_leapp(y+1) && mo==1);
		md2b = md2b >= 0 ? md2b : 1;

		ansp[i] = m != NA_INTEGER ? _mkFDate(y+1, mo+1, md2b) : NA_INTEGER;
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
week_bang_FDate(SEXP x, SEXP value)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const int *xp = INTEGER(x);
	const int *vp = INTEGER(value);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		static const int_fast8_t iso[] = {2, 1, 0, -1, -2, 4, 3, 2};
		int m = xp[i];
		unsigned int w2b = vp[i];
		unsigned int y = m / 391U;
		unsigned int yd = m % 391U;
		unsigned int md = (yd + 192U) % 195U % 97U % 32U;
		unsigned int mo = (yd - md) / 32U;

		if (m != NA_INTEGER && w2b <= 53U && yd && md) {
			unsigned int eo = yday_eom[mo + 1U];
			/* f01 is the wday of Jan-01 */
			unsigned int f01 = (y + y / 4U - y / 100U + y / 400U + 1U) % 7U;
			unsigned int ly = _leapp(y+1);
			unsigned int w;
			int yd2b;

			md += mo>2U && ly;
			eo += mo>1U && ly;

			yd = yday_eom[mo] + md;
			yd = yd <= eo ? yd : eo;
			w = (7 + yd - iso[f01]) / 7;
			yd2b = yd + (w2b - w) * 7 - 1;

			if (yd2b < 0) {
				ly = _leapp(y--);
				yd2b += 365 + ly;
			} else if (yd2b >= 365 + ly) {
				yd2b -= 365 + ly;
				ly = _leapp(++y + 1);
			}

			/* first and second trimester */
			yd2b -= ly;
			/* second trimester */
			yd2b += (yd2b >= 151 + 61) << 5U;
			/* third and first trimester */
			yd2b += (yd2b >= 59 && yd2b < 151 + 61) << 1U;
			yd2b += ly && yd2b < 59;

			mo = 2 * yd2b / 61;
			md = 2 * yd2b % 61;
			md >>= 1U;

			ansp[i] = _mkFDate(y+1, mo+1 - (mo>=7), md+1);
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
wday_bang_FDate(SEXP x, SEXP value)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const int *xp = INTEGER(x);
	const int *vp = INTEGER(value);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		static const int_fast8_t iso[] = {2, 1, 0, -1, -2, 4, 3, 2};
		int m = xp[i];
		unsigned int wd2b = vp[i];
		unsigned int y = m / 391U;
		unsigned int yd = m % 391U;
		unsigned int md = (yd + 192U) % 195U % 97U % 32U;
		unsigned int mo = (yd - md) / 32U;

		if (m != NA_INTEGER && wd2b <= 7U && yd && md) {
			unsigned int eo = yday_eom[mo + 1U];
			/* f00 is the wday of Jan-00 */
			unsigned int f00 = (y + y / 4U - y / 100U + y / 400U);
			unsigned int ly = _leapp(y+1);
			unsigned int wd;
			int yd2b;

			md += mo>1U && ly;
			eo += mo>0U && ly;

			yd = yday_eom[mo] + md;
			yd = yd <= eo ? yd : eo;
			/* sun is 7U, this is wd2b + 6 % 7 - (f00 + yd - 1) % 7*/
			wd = (f00 + yd) % 7U;
			wd2b ^= -!wd2b & 0x7U;
			yd2b = yd + (wd2b - wd) - 1;

			if (yd2b < 0) {
				ly = _leapp(y--);
				yd2b += 365 + ly;
			} else if (yd2b >= 365 + ly) {
				yd2b -= 365 + ly;
				ly = _leapp(++y + 1);
			}

			/* first and second trimester */
			yd2b -= ly;
			/* second trimester */
			yd2b += (yd2b >= 151 + 61) << 5U;
			/* third and first trimester */
			yd2b += (yd2b >= 59 && yd2b < 151 + 61) << 1U;
			yd2b += ly && yd2b < 59;

			mo = 2 * yd2b / 61;
			md = 2 * yd2b % 61;
			md >>= 1U;

			ansp[i] = _mkFDate(y+1, mo+1 - (mo>=7), md+1);
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
wcnt_FDate_bang(SEXP x, SEXP value)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(WCNTSXP, n));
	int *restrict ansp = WCNT(ans);
	const int *xp = INTEGER(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = xp[i];
		unsigned int y = m / 391U;
		unsigned int yd = m % 391U;
		unsigned int md = (yd + 192U) % 195U % 97U % 32U;
		unsigned int mo = (yd - md) / 32U;

		if (LIKELY(m != NA_INTEGER && yd && md)) {
			unsigned int eo = yday_eom[mo + 1U];
			/* f00 is the wday of Jan-00 */
			unsigned int f00 = y + y / 4U - y / 100U + y / 400U;
			unsigned int wc;
			unsigned int wd;

			md += mo>1U && _leapp(y+1U);
			eo += mo>0U && _leapp(y+1U);

			yd = yday_eom[mo] + md;
			yd = yd <= eo ? yd : eo;

			wc = (yd - 1U) / 7U + 1U;
			wd = (f00 + yd - 1U) % 7U + 1U;
			ansp[i] = wc << 3 ^ wd;
		} else {
			ansp[i] = NA_INTEGER;
		}
	}

	with (SEXP class) {
		PROTECT(class = allocVector(STRSXP, 2));
		SET_STRING_ELT(class, 0, mkChar("wcnt"));
		SET_STRING_ELT(class, 1, mkChar(".duo"));
		classgets(ans, class);
	}

	UNPROTECT(2);
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
	const int c = cmp_ui(fd, td);
	/* decomp */
	FDate old = fd;
	unsigned int y = fd / 391U;
	unsigned int yd = fd % 391U;
	int md = (yd + 192U) % 195U % 97U % 32U;
	int mo = (yd - md) / 32U;

	if (!(yd && md) && !d.d) {
		/* special dates, only allowed for d.d == 0 */
		unsigned int qd = (yd % 97U - (yd > 195U));

		if (d.m) {
			switch (qd%4U) {
			case 0U:
				mo = md = 0;
				md -= !(d.m % 12);
			case 1U:
				md -= !(d.m % 6);
			case 2U:
				md -= !(d.m % 3);
			case 3U:
				break;
			}
		} else {
			switch (qd%4U) {
			case 0U:
				d.m = 12;
				mo = 0, md = -3;
				break;
			case 1U:
				d.m = 6;
				md = -2;
				break;
			case 2U:
				d.m = 3;
				md = -1;
				break;
			case 3U:
				d.m = 1;
				break;
			}
			d.m *= c;
		}

		/* we use at least month steps */
		with (unsigned int m = fd < td ? td - fd : fd - td) {
			m >>= 5U;
			tmp = Calloc(m + 1U, FDate);
		}
		if (c) {
			/* because of auto-fixups above */
			fd = _mkFDate(y+1U, mo+1U, md);
			do {
				/* what we've got in the last round */
				tmp[z++] = fd;

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

				switch (qd%4U) {
				case 0U:
					mo = md = 0;
					md--;
				case 1U:
					md--;
				case 2U:
					md--;
				case 3U:
					break;
				}

				/* the exit condition is equivalent to
				 * old < fd < td  if the original fd < td  and
				 * old > fd > td  if the original fd > td
				 * equality is handled after this if-block */
			} while (cmp_ui(fd, td) == c && cmp_ui(old, fd) == c);
			/* make sure we deal with empty sums */
			z -= cmp_ui(fd, old) == c;
		}
	} else if (c) {
		d.d += c * (!d.d && !d.m);
		with (unsigned int m = fd < td ? td - fd : fd - td) {
			/* if only month steps, save on allocation */
			m >>= !d.d * 5U;
			tmp = Calloc(m + 1U, FDate);
		}
		/* maybe spurious? */
		mo &= -!!yd;
		md &= -!!(yd && md);
		md += !md && d.d < 0;
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
		} while (cmp_ui(fd, td) == c && cmp_ui(old, fd) == c);
		/* make sure we deal with empty sums */
		z -= cmp_ui(fd, old) == c;
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

		if (UNLIKELY(m == NA_INTEGER || _is_na_ddur(d))) {
			ansp[i] = NA_INTEGER;
			continue;
		} else if (!(yd && md) && !d.d) {
			unsigned int qd = (yd % 97U - (yd > 195U));

			switch (qd%4U) {
			case 0U:
				mo = md = 0;
				md -= !(d.m % 12);
			case 1U:
				md -= !(d.m % 6);
			case 2U:
				md -= !(d.m % 3);
			case 3U:
				break;
			}
			y += (mo + d.m) / 12;
			mo = (mo + d.m) % 12;
			/* make sure residues are non-negative */
			y -= mo < 0;
			mo = (mo + 12) % 12;
			goto out;
		} else if (!(yd && md)) {
			/* maybe spurious? */
			mo &= -!!yd;
			md = d.d < 0;
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
as_ddur_factor(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(DDURSXP, n));
	ddur *restrict ansp = DDUR(ans);
	const int *xp = INTEGER(x);
	const SEXP *lvl;
	R_len_t nlvl;

	with (SEXP levels = getAttrib(x, R_LevelsSymbol)) {
		if (isNull(levels)) {
			error("Factor vector with no levels");
		}
		lvl = STRING_PTR_RO(levels);
		nlvl = length(levels);
	}

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int l = xp[i];
		ddur d;

		if (UNLIKELY(l == NA_INTEGER || (unsigned int)--l >= nlvl ||
			     (d = _rdddur(CHAR(lvl[l]))).d == NA_INTEGER)) {
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
as_ddur_wcnt(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(DDURSXP, n));
	ddur *restrict ansp = DDUR(ans);
	const wcnt *xp = WCNT(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		wcnt v = xp[i];

		ansp[i] = v != NA_INTEGER && !(v & 0x7U) ? (ddur){(v >> 3) * 7} : NA_DDUR;
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
year_bang_ddur(SEXP x, SEXP value)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(DDURSXP, n));
	ddur *restrict ansp = DDUR(ans);
	const ddur *xp = DDUR(x);
	const int *vp = INTEGER(value);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		ddur m = xp[i];
		int y2b = vp[i];

		ansp[i] = !_is_na_ddur(m) ? (ddur){m.d, y2b * 12} : NA_DDUR;
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
month_bang_ddur(SEXP x, SEXP value)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(DDURSXP, n));
	ddur *restrict ansp = DDUR(ans);
	const ddur *xp = DDUR(x);
	const int *vp = INTEGER(value);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		ddur m = xp[i];
		int m2b = vp[i];

		ansp[i] = !_is_na_ddur(m) ? (ddur){m.d, m2b} : NA_DDUR;
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
week_bang_ddur(SEXP x, SEXP value)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(DDURSXP, n));
	ddur *restrict ansp = DDUR(ans);
	const ddur *xp = DDUR(x);
	const int *vp = INTEGER(value);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		ddur m = xp[i];
		int w2b = vp[i];

		ansp[i] = !_is_na_ddur(m) ? (ddur){w2b * 7, m.m} : NA_DDUR;
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
dday_bang_ddur(SEXP x, SEXP value)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(DDURSXP, n));
	ddur *restrict ansp = DDUR(ans);
	const ddur *xp = DDUR(x);
	const int *vp = INTEGER(value);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		ddur m = xp[i];
		int d2b = vp[i];

		ansp[i] = !_is_na_ddur(m) ? (ddur){d2b, m.m} : NA_DDUR;
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
seq_ddur(SEXP from, SEXP till, SEXP by)
{
	ddur fd = DDUR_ELT(from, 0U);
	ddur td = DDUR_ELT(till, 0U);
	ddur d = DDUR_ELT(by, 0U);
	SEXP ans;
	ddur *tmp;
	size_t z = 0U;
	const int c = cmp_ddur(fd, td);
	/* decomp */
	ddur old = fd;

	/* determine BY if omitted */
	d.d += c * (!d.d && !d.m && fd.m == td.m);
	d.m += c * (!d.d && !d.m);
	if (!c || UNLIKELY(!d.m && fd.m != td.m)) {
		tmp = Calloc(1U, ddur);
	} else {
		size_t m = d.m ? iabs(td.m - fd.m) : iabs(td.d - fd.d);
		/* if only month steps, save on allocation */
		tmp = Calloc(m + 2U, ddur);

		do {
			/* what we've got in the last round */
			tmp[z++] = fd;

			fd.m += d.m;
			fd.d += d.d;

			/* the exit condition is equivalent to
			 * old < fd < td  if the original fd < td  and
			 * old > fd > td  if the original fd > td
			 * equality is handled after this if-block */
		} while (z <= m && cmp_ddur(fd, td) == c && cmp_ddur(old, fd) == c);
		/* make sure we deal with empty sums */
		z -= cmp_ddur(fd, old) == c;
	}
	/* seq is inclusive, so finalise tmp if they're equal */
	tmp[z] = fd;
	z += fd.m == td.m && fd.d == td.d;

	/* now for real */
	ans = PROTECT(allocVector(DDURSXP, z));
	memcpy(DDUR(ans), tmp, z * sizeof(*tmp));
	Free(tmp);

	with (SEXP class = PROTECT(allocVector(STRSXP, 2))) {
		SET_STRING_ELT(class, 0, mkChar("ddur"));
		SET_STRING_ELT(class, 1, mkChar(".duo"));
		classgets(ans, class);
	}

	UNPROTECT(2);
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

		if (LIKELY(!_is_na_ddur(d))) {
			d.m = -d.m;
			d.d = -d.d;
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

		if (LIKELY(!_is_na_ddur(d))) {
			d.m = (int)((double)d.m * yp[i]);
			d.d = (int)((double)d.d * yp[i]);
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

		if (LIKELY(!_is_na_ddur(d))) {
			d.m = (int)((double)d.m / yp[i]);
			d.d = (int)((double)d.d / yp[i]);
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

		if (LIKELY(!_is_na_ddur(d))) {
			d.m = (int)(fmod((double)d.m, yp[i]));
			d.d = (int)(fmod((double)d.d, yp[i]));
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
lt_ddur(SEXP x, SEXP y)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(LGLSXP, n));
	int *restrict ansp = LOGICAL(ans);
	const ddur *xp = DDUR(x);
	const ddur *yp = DDUR(y);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		ddur xd = xp[i];
		ddur yd = yp[i];

		ansp[i] = !_is_na_ddur(xd) && !_is_na_ddur(yd)
			? xd.m * 61LL + xd.d * 2LL + (xd.m<yd.m) < yd.m * 61LL + yd.d * 2LL + (yd.m<xd.m)
			: NA_LOGICAL;
	}

	UNPROTECT(1);
	return ans;
}

SEXP
le_ddur(SEXP x, SEXP y)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(LGLSXP, n));
	int *restrict ansp = LOGICAL(ans);
	const ddur *xp = DDUR(x);
	const ddur *yp = DDUR(y);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		ddur xd = xp[i];
		ddur yd = yp[i];

		ansp[i] = !_is_na_ddur(xd) && !_is_na_ddur(yd)
			? xd.m * 61LL + xd.d * 2LL + (xd.m<yd.m) <= yd.m * 61LL + yd.d * 2LL + (yd.m<xd.m)
			: NA_LOGICAL;
	}

	UNPROTECT(1);
	return ans;
}

SEXP
gt_ddur(SEXP x, SEXP y)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(LGLSXP, n));
	int *restrict ansp = LOGICAL(ans);
	const ddur *xp = DDUR(x);
	const ddur *yp = DDUR(y);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		ddur xd = xp[i];
		ddur yd = yp[i];

		ansp[i] = !_is_na_ddur(xd) && !_is_na_ddur(yd)
			? xd.m * 61LL + xd.d * 2LL + (xd.m<yd.m) > yd.m * 61LL + yd.d * 2LL + (yd.m<xd.m)
			: NA_LOGICAL;
	}

	UNPROTECT(1);
	return ans;
}

SEXP
ge_ddur(SEXP x, SEXP y)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(LGLSXP, n));
	int *restrict ansp = LOGICAL(ans);
	const ddur *xp = DDUR(x);
	const ddur *yp = DDUR(y);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		ddur xd = xp[i];
		ddur yd = yp[i];

		ansp[i] = !_is_na_ddur(xd) && !_is_na_ddur(yd)
			? xd.m * 61LL + xd.d * 2LL + (xd.m<yd.m) >= yd.m * 61LL + yd.d * 2LL + (yd.m<xd.m)
			: NA_LOGICAL;
	}

	UNPROTECT(1);
	return ans;
}


SEXP
as_wcnt_character(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(WCNTSXP, n));
	wcnt *restrict ansp = WCNT(ans);
	const SEXP *xp = STRING_PTR_RO(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		SEXP s = xp[i];
		wcnt d;

		if (UNLIKELY(s == NA_STRING)) {
			d = NA_WCNT;
		} else {
			d = _rdwcnt(CHAR(s));
		}

		ansp[i] = d;
	}

	with (SEXP class) {
		PROTECT(class = allocVector(STRSXP, 2));
		SET_STRING_ELT(class, 0, mkChar("wcnt"));
		SET_STRING_ELT(class, 1, mkChar(".duo"));
		classgets(ans, class);
	}

	UNPROTECT(2);
	return ans;
}

SEXP
as_wcnt_factor(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(WCNTSXP, n));
	wcnt *restrict ansp = WCNT(ans);
	const int *xp = INTEGER(x);
	const SEXP *lvl;
	R_len_t nlvl;

	with (SEXP levels = getAttrib(x, R_LevelsSymbol)) {
		if (isNull(levels)) {
			error("Factor vector with no levels");
		}
		lvl = STRING_PTR_RO(levels);
		nlvl = length(levels);
	}

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int l = xp[i];
		wcnt d;

		if (UNLIKELY(l == NA_INTEGER || (unsigned int)--l >= nlvl)) {
			d = NA_WCNT;
		} else {
			d = _rdwcnt(CHAR(lvl[l]));
		}

		ansp[i] = d;
	}

	with (SEXP class) {
		PROTECT(class = allocVector(STRSXP, 2));
		SET_STRING_ELT(class, 0, mkChar("wcnt"));
		SET_STRING_ELT(class, 1, mkChar(".duo"));
		classgets(ans, class);
	}

	UNPROTECT(2);
	return ans;
}

SEXP
format_wcnt(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(STRSXP, n));
	const wcnt *xp = WCNT(x);

	/* no omp here as mkCharLen doesn't like it */
	for (R_xlen_t i = 0; i < n; i++) {
		char buf[64U];

		if (LIKELY(xp[i] != NA_WCNT)) {
			SET_STRING_ELT(ans, i, mkCharLen(buf, _prwcnt(buf, sizeof(buf), xp[i])));
		} else {
			SET_STRING_ELT(ans, i, NA_STRING);
		}
	}
	UNPROTECT(1);
	return ans;
}

SEXP
plus_wcnt(SEXP x, SEXP y)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(WCNTSXP, n));
	wcnt *restrict ansp = WCNT(ans);
	const wcnt *xp = WCNT(x);
	const ddur *yp = DDUR(y);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		wcnt w = xp[i];
		ddur d = yp[i];

		if (LIKELY(w != NA_INTEGER && !_is_na_ddur(d) && !d.m)) {
			int c = w >> 3;
			int wd = w & 0x7U;

			c += d.d / 7;
			wd += d.d % 7;
			c += wd >> 3;
			wd &= 0x7U;

			ansp[i] = c << 3U ^ wd;
		} else {
			ansp[i] = NA_INTEGER;
		}
	}

	with (SEXP class) {
		PROTECT(class = allocVector(STRSXP, 2));
		SET_STRING_ELT(class, 0, mkChar("wcnt"));
		SET_STRING_ELT(class, 1, mkChar(".duo"));
		classgets(ans, class);
	}

	UNPROTECT(2);
	return ans;
}

SEXP
week_wcnt(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const wcnt *xp = WCNT(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = xp[i];

		ansp[i] = m != NA_INTEGER ? m >> 3 : NA_INTEGER;
	}

	UNPROTECT(1);
	return ans;
}

SEXP
wday_wcnt(SEXP x)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(INTSXP, n));
	int *restrict ansp = INTEGER(ans);
	const wcnt *xp = WCNT(x);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = xp[i];

		ansp[i] = m != NA_INTEGER ? m & 0x7U : NA_INTEGER;
	}

	UNPROTECT(1);
	return ans;
}

SEXP
week_bang_wcnt(SEXP x, SEXP value)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(WCNTSXP, n));
	int *restrict ansp = WCNT(ans);
	const int *xp = WCNT(x);
	const int *vp = INTEGER(value);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = xp[i];
		int v = vp[i];

		ansp[i] = m != NA_INTEGER && v != NA_INTEGER
			? (v >> 3) << 3 ^ (m & 0x7U)
			: NA_INTEGER;
	}

	with (SEXP class) {
		PROTECT(class = allocVector(STRSXP, 2));
		SET_STRING_ELT(class, 0, mkChar("wcnt"));
		SET_STRING_ELT(class, 1, mkChar(".duo"));
		classgets(ans, class);
	}

	UNPROTECT(2);
	return ans;
}

SEXP
wday_bang_wcnt(SEXP x, SEXP value)
{
	R_xlen_t n = XLENGTH(x);
	SEXP ans = PROTECT(allocVector(WCNTSXP, n));
	int *restrict ansp = WCNT(ans);
	const int *xp = WCNT(x);
	const int *vp = INTEGER(value);

	#pragma omp parallel for
	for (R_xlen_t i = 0; i < n; i++) {
		int m = xp[i];
		unsigned int v = vp[i];

		ansp[i] = m != NA_INTEGER && v != NA_INTEGER
			? (m >> 3) << 3 ^ (v & 0x7U)
			: NA_INTEGER;
	}

	with (SEXP class) {
		PROTECT(class = allocVector(STRSXP, 2));
		SET_STRING_ELT(class, 0, mkChar("wcnt"));
		SET_STRING_ELT(class, 1, mkChar(".duo"));
		classgets(ans, class);
	}

	UNPROTECT(2);
	return ans;
}
