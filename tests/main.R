library(data.table)
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

year.d <- c(
	1999L, 1999L,
	1999L, 1999L, 1999L,
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
	2000L, 2000L,
	2000L, 2000L,
	2001L, 2001L,
	2001L, NA_integer_,
	2001L, 2001L)

yday.d <- c(
	307L, 337L,
	338L, 365L, 366L,
	1L, 31L,
	32L, 61L,
	62L, 92L,
	93L, 122L,
	123L, 153L,
	154L, 184L,
	185L, 214L,
	215L, 245L,
	246L, 275L,
	276L, 306L,
	307L, 337L,
	338L, 365L,
	1L, 31L,
	32L, NA_integer_,
	62L, 92L)

month.d <- c(
	11L, 11L,
	12L, 12L, 12L,
	1L, 1L,
	2L, 2L,
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
	1L, 1L,
	2L, NA_integer_,
	3L, 3L)

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

bigd <- rep(as.IDate(d), 40000)
library(microbenchmark)
print(microbenchmark(year(bigd)))
print(microbenchmark(yday(bigd)))
print(microbenchmark(month(bigd)))
print(microbenchmark(mday(bigd)))
