library(data.table)
library(dateutils)

x <- c("2000-A", "2000-S1", "2000-Q1", "2000-01", "2000-01-01",
	"2000-02-00", "2000-02-01", "2000-02-28", "2000-02-29", "2000-02-30", "2000-02-31",
	"2000-3-0", "2000/3/1", "2000 Q2", "2000-04",
	"2001-02-28", "2001-02-29", "2001-02-30", "2001-02-31",
	"2001-Q3", "2001-S2", "2001-Q4",
	"2002-01", "2002-02", "2002-03", "2002-04",
	"2002-05", "2002-06", "2002-07", "2002-08",
	"2002-09", "2002-10", "2002-11", "2002-12")
print(data.table(x, as.FDate(x), as.EDate(as.FDate(x))))

y <- seq(unclass(as.FDate("2002-A")),unclass(as.FDate("2003-A")))
y <- seq(unclass(as.FDate("2000-A")),unclass(as.FDate("2001-A")))
class(y) <- c("FDate",".duo")

options(width=196)
options(datatable.print.nrows=400)

print(data.table(y, year=year(y), yday=yday(y), semi=semi(y), sday=sday(y), quarter=quarter(y), qday=qday(y), month=month(y), mday=mday(y), week=week(y), wday=wday(y)))

print("")
print(as.FDate("20140102"))
print(as.FDate("20140132"))
print(as.FDate("20141302"))
print(as.FDate("2010402"))
print(as.FDate(20140102L))
print(as.FDate(20140132L))
print(as.FDate(20141302L))
print(as.FDate(2010402L))

if (FALSE) {
library(microbenchmark)

print(length(bigy <- rep(y, 100000L)))
print(class(bigy))
print("micro")
print(microbenchmark(yday(bigy)))
print(microbenchmark(sday(bigy)))
print(microbenchmark(qday(bigy)))
print(microbenchmark(mday(bigy)))
print(microbenchmark(wday(bigy)))

library(lubridate)

print(microbenchmark(as.FDate(rep("2000-02-29",100000L))))
print(microbenchmark(ymd(rep("2000-02-29",100000L))))
print(microbenchmark(as.Date(rep("2000-02-29",100000L))))

x <- as.FDate(rep("2000-02-29",100000L))
print(microbenchmark(format(x)))
x <- ymd(rep("2000-02-29",100000L))
print(microbenchmark(format(x)))
x <- as.Date(rep("2000-02-29",100000L))
print(microbenchmark(format(x)))
}
