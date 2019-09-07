#include <unistd.h>
#include <sys/socket.h>
#include <sys/epoll.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>
#include <string.h>
#include <R.h>
#include "Rdateutils.h"
#include "nifty.h"

static inline MDate
_j00(unsigned int y)
{
	return y * 365U + y / 4U - y / 100U + y / 400U;
}

static inline unsigned int
_year(MDate x)
{
	unsigned int guess = x / 365U;
	guess -= _j00(guess) > x;
	guess -= _j00(guess) > x;
	return guess;
}

static inline int
_yday(MDate x)
{
	unsigned int y = _year(x);
	return x - _j00(y) + 1;
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
		INTEGER(ans)[i] = _year(m);
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
		INTEGER(ans)[i] = _yday(m);
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
		int yd = _yday(m);
		int pent = yd / 153;
		int pend = yd % 153;
		int mo = pend % 61;
		INTEGER(ans)[i] = 5 * pend + mo + 1;
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
		int yd = _yday(m);
		int pent = yd / 153;
		int pend = yd % 153;
		int md = (pend % 61) / 2;
		INTEGER(ans)[i] = md + 1;
	}

	UNPROTECT(1);
	return ans;
}
