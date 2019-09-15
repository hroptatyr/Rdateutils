year <- function(x, ...) UseMethod("year")
yday <- function(x, ...) UseMethod("yday")
semi <- function(x, ...) UseMethod("semi")
sday <- function(x, ...) UseMethod("sday")
quarter <- function(x, ...) UseMethod("quarter")
qday <- function(x, ...) UseMethod("qday")
month <- function(x, ...) UseMethod("month")
mday <- function(x, ...) UseMethod("mday")
week <- function(x, ...) UseMethod("week")
wday <- function(x, ...) UseMethod("wday")


as.EDate <- function(x, ...) UseMethod("as.EDate")

as.EDate.default <- function(x, ..., tz=attr(x, "tzone"))
{
	as.EDate(as.Date(x, tz=if(!is.null(tz)) tz else "UTC", ...))
}

as.EDate.character <- function(x)
{
	.Call(Cas.EDate.character, as.character(x))
}

as.EDate.Date <- as.EDate.IDate <- function(x, ...)
{
	x <- as.integer(x) + 719469L ##/*0000-03-00*/
	class(x) <- "EDate"
	return(x)
}

as.EDate.POSIXct <- function(x, tz=attr(x, "tzone"), ...)
{
	if (is.null(tz) || tz == "UTC") {
		x <- as.integer(x / 86400)
		class(x) <- "EDate"
		return(x)
	}
	return(as.EDate(as.Date(x, tz=tz, ...)))
}

as.EDate.EDate <- function(x, ...)
{
	return(x)
}

as.EDate.numeric <- function(x, ...)
{
## assume correct origin
	x <- as.integer(x)
	class(x) <- "EDate"
	return(x)
}

as.Date.EDate <- function(x, ...)
{
	x <- as.numeric(x - 719469L)
	class(x) <- "Date"
	return(x)
}

as.IDate.EDate <- function(x, ...)
{
	x <- as.integer(x) - 719469L
	class(x) <- c("IDate", "Date")
	return(x)
}

as.POSIXlt.EDate <- function(x, ...)
{
	.Call(Cas.POSIXlt.EDate, x);
}

as.POSIXct.EDate <- function(x, ...)
{
	return(as.POSIXct(as.Date(x)))
}

format.EDate <- function(x, ...)
{
	.Call(Cformat.EDate, x)
}

print.EDate <- function(x, ...)
{
	print(format.EDate(x), ...)
}

c.EDate <- cut.EDate <- mean.EDate <- rep.EDate <-
cut.EDate <- mean.EDate <- rep.EDate <- round.EDate <-
seq.EDate <- split.EDate <- unique.EDate <- function(x, ...)
{
	as.EDate(NextMethod())
}

## accessors
year.EDate <- function(x)
{
	.Call(Cyear.EDate, x)
}

`year<-` <- function(x, value)
{
	.Call(`Cyear<-`, as.EDate(x), rep_len(as.integer(value), length(x)))
}

yday.EDate <- function(x)
{
	.Call(Cyday.EDate, x)
}

`yday<-` <- function(x, value)
{
	.Call(`Cyday<-`, as.EDate(x), rep_len(as.integer(value), length(x)))
}

semi.EDate <- function(x)
{
	.Call(Csemi.EDate, x)
}

sday.EDate <- function(x)
{
	.Call(Csday.EDate, x)
}

quarter.EDate <- function(x)
{
	.Call(Cquarter.EDate, x)
}

qday.EDate <- function(x)
{
	.Call(Cqday.EDate, x)
}

month.EDate <- function(x)
{
	.Call(Cmonth.EDate, x)
}

`month<-` <- function(x, value)
{
	.Call(`Cmonth<-`, as.EDate(x), rep_len(as.integer(value), length(x)))
}

mday.EDate <- function(x)
{
	.Call(Cmday.EDate, x)
}

`mday<-` <- function(x, value)
{
	.Call(`Cmday<-`, as.EDate(x), rep_len(as.integer(value), length(x)))
}

week.EDate <- function(x)
{
	.Call(Cweek.EDate, x)
}

wday.EDate <- function(x)
{
	.Call(Cwday.EDate, x)
}


as.FDate <- function(x, ...) UseMethod("as.FDate")

as.FDate.character <- function(x)
{
	.Call(Cas.FDate.character, as.character(x))
}

as.FDate.FDate <- function(x, ...)
{
	return(x)
}

as.FDate.numeric <- function(x, ...)
{
## assume correct origin
	x <- as.integer(x)
	class(x) <- "FDate"
	return(x)
}

format.FDate <- function(x, ...)
{
	.Call(Cformat.FDate, x)
}

print.FDate <- function(x, ...)
{
	print(format.FDate(x), ...)
}

as.POSIXlt.FDate <- function(x)
{
	.Call(Cas.POSIXlt.FDate, x)
}

c.FDate <- cut.FDate <- mean.FDate <- rep.FDate <-
cut.FDate <- mean.FDate <- rep.FDate <- round.FDate <-
seq.FDate <- split.FDate <- unique.FDate <- function(x, ...)
{
	as.FDate(NextMethod())
}

## accessors
year.FDate <- function(x)
{
	.Call(Cyear.FDate, x)
}

yday.FDate <- function(x)
{
	.Call(Cyday.FDate, x)
}

semi.FDate <- function(x)
{
	.Call(Csemi.FDate, x)
}

sday.FDate <- function(x)
{
	.Call(Csday.FDate, x)
}

quarter.FDate <- function(x)
{
	.Call(Cquarter.FDate, x)
}

qday.FDate <- function(x)
{
	.Call(Cqday.FDate, x)
}

month.FDate <- function(x)
{
	.Call(Cmonth.FDate, x)
}

mday.FDate <- function(x)
{
	.Call(Cmday.FDate, x)
}

week.FDate <- function(x)
{
	.Call(Cweek.FDate, x)
}

wday.FDate <- function(x)
{
	.Call(Cwday.FDate, x)
}
