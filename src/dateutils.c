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
		unsigned int y = (yyd >> 16U) + ((yyd & 0xfffU) >= 307U);
		INTEGER(ans)[i] = m != NA_INTEGER ? y : NA_INTEGER;
	}

	UNPROTECT(1);
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
		INTEGER(ans)[i] = m != NA_INTEGER ? _yday(m) : NA_INTEGER;
	}

	UNPROTECT(1);
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
		unsigned int yd = _yday(m) - 1;
		int pent = yd / 153;
		int pend = yd % 153;
		int mo = 2 * pend / 61;
		INTEGER(ans)[i] = m != NA_INTEGER ? 5 * pent + mo + 1 : NA_INTEGER;
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
		unsigned int yd = _yday(m) - 1;
		int pend = yd % 153;
		int md = (2 * pend % 61) / 2;
		INTEGER(ans)[i] = m != NA_INTEGER ? md + 1 : NA_INTEGER;
	}

	UNPROTECT(1);
	return ans;
}
