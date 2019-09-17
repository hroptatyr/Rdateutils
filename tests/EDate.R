library(data.table)
library(dateutils)

y <- seq(as.EDate("2002-01-00"),as.EDate("2003-01-01"))
y <- seq(as.EDate("2000-01-00"),as.EDate("2002-01-01"))

options(width=196)
options(datatable.print.nrows=800)

print(data.table(y, year=year(y), yday=yday(y), semi=semi(y), sday=sday(y), quarter=quarter(y), qday=qday(y), month=month(y), mday=mday(y), week=week(y), wday=wday(y)))

f <- as.EDate("2000-03-31")
month(f) <- 1
print(f)
month(f) <- 4
print(f)
month(f) <- 2
print(f)
month(f) <- 12
print(f)

print("")
x <- as.EDate(c("2014-01-02", "2013-02-02","2015-01-06"))
print(min(x))
print(max(x))
print(range(x))
