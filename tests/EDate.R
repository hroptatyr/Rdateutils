library(data.table)
library(dateutils)

y <- seq(as.EDate("2002-01-00"),as.EDate("2003-01-01"))
y <- seq(as.EDate("2000-01-00"),as.EDate("2001-01-01"))

options(width=196)
options(datatable.print.nrows=400)

print(data.table(y, year=year(y), yday=yday(y), semi=semi(y), sday=sday(y), quarter=quarter(y), qday=qday(y), month=month(y), mday=mday(y), week=week(y), wday=wday(y)))
