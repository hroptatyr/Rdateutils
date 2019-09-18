library(dateutils)
library(microbenchmark)

x <- format(seq(from=as.Date("100-01-01"), to=as.Date("3299-12-31"), by=1))
print(length(x))
print(microbenchmark(as.FDate(x)))
print(microbenchmark(as.EDate(x)))

y <- as.FDate(x)
print(microbenchmark(format(y)))
y <- as.EDate(x)
print(microbenchmark(format(y)))
