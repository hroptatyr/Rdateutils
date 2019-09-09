library(dateutils)

d <- c(
	"2000-01-01", "2000-01-31",
	"2000-02-01", "2000-02-28", "2000-02-29",
	"2000-03-01", "2000-03-31",
	"2000-04-01", "2000-04-30",
	"2000-05-01", "2000-05-31",
	"2000-06-01", "2000-06-30",
	"2000-07-01", "2000-07-31",
	"2000-08-01", "2000-08-31",
	"2000-09-01", "2000-09-30",
	"2000-10-01", "2000-10-31",
	"2000-11-01", "2000-11-30",
	"2000-12-01", "2000-12-31",
	"2001-01-01", "2001-01-31",
	"2001-02-01", "2001-02-28",
	"2001-03-01", "2001-03-31",
	"2001-04-01", "2001-04-31",
	"2001-05-01", "2001-05-31")

d <- as.EDate(d)

year.d <- c(
	2000L, 2000L,
	2000L, 2000L, 2000L,
	2000L, 2000L,
	2000L, 2000L,
	2000L, 2000L,
	2000L, 2000L,
	2000L, 2000L,
	2000L, 2000L,
	2000L, 2000L,
	2000L, 2000L,
	2000L, 2000L,
	2000L, 2000L,
	2001L, 2001L,
	2001L, 2001L,
	2001L, 2001L,
	2001L, NA_integer_,
	2001L, 2001L)

yday.d <- c(
	1L, 31L,
	32L, 59L, 60L,
	61L, 91L,
	92L, 121L,
	122L, 152L,
	153L, 182L,
	183L, 213L,
	214L, 244L,
	245L, 274L,
	275L, 305L,
	306L, 335L,
	336L, 366L,
	1L, 31L,
	32L, 59L,
	60L, 90L,
	91L, NA_integer_,
	121L, 151L)

month.d <- c(
	01L, 01L,
	02L, 02L, 02L,
	3L, 3L,
	4L, 4L,
	5L, 5L,
	6L, 6L,
	7L, 7L,
	8L, 8L,
	9L, 9L,
	10L, 10L,
	11L, 11L,
	12L, 12L,
	01L, 01L,
	02L, 02L,
	3L, 3L,
	4L, NA_integer_,
	5L, 5L)

mday.d <- c(
	1L, 31L,
	1L, 28L, 29L,
	1L, 31L,
	1L, 30L,
	1L, 31L,
	1L, 30L,
	1L, 31L,
	1L, 31L,
	1L, 30L,
	1L, 31L,
	1L, 30L,
	1L, 31L,
	1L, 31L,
	1L, 28L,
	1L, 31L,
	1L, NA_integer_,
	1L, 31L)

print(all(!is.na(year.d) & year(d) == year.d | is.na(year(d))))
print(all(!is.na(yday.d) & yday(d) == yday.d | is.na(yday(d))))
print(all(!is.na(month.d) & month(d) == month.d | is.na(month(d))))
print(all(!is.na(mday.d) & mday(d) == mday.d | is.na(mday(d))))

print("")
e <- d
print(as.POSIXlt({year(e) <- 2000;e}))
print(as.POSIXlt({year(e) <- 2001;e}))
print("")
e <- d
print(as.POSIXlt({yday(e) <- 1;e}))
print(as.POSIXlt({yday(e) <- 60;e}))
print("")
e <- d
print(as.POSIXlt({month(e) <- 2;e}))
e <- d
print(as.POSIXlt({month(e) <- 11;e}))
print("")
e <- d
print(as.POSIXlt({mday(e) <- 15;e}))
e <- d
print(as.POSIXlt({mday(e) <- 31;e}))

bigd <- rep(as.EDate(d), 40000)
library(microbenchmark)
print(microbenchmark(year(bigd)))
print(microbenchmark(yday(bigd)))
print(microbenchmark(month(bigd)))
print(microbenchmark(mday(bigd)))

print(microbenchmark(as.POSIXlt(bigd)))
print(microbenchmark(as.POSIXlt(as.Date(bigd))))
