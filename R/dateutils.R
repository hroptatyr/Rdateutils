as.EDate <- function(x, ...) UseMethod("as.EDate")

as.EDate.default <- function(x, ..., tz=attr(x, "tzone"))
{
	as.EDate(as.Date(x, tz=if(!is.null(tz)) tz else "UTC", ...))
}

as.EDate.Date <- as.EDate.IDate <- function(x, ...)
{
	x <- as.integer(x) + 719468L ##/*0000-03-00*/
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
	x <- as.numeric(x - 719468L)
	class(x) <- "Date"
	return(x)
}

as.IDate.EDate <- function(x, ...)
{
	x <- as.integer(x) - 719468L
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


c.EDate <- cut.EDate <- mean.EDate <- rep.EDate <-
cut.EDate <- mean.EDate <- rep.EDate <- round.EDate <-
seq.EDate <- split.EDate <- unique.EDate <- function(x, ...)
{
	as.EDate(NextMethod())
}


## extractions
year <- function(x)
{
	.Call(Cyear, as.EDate(x))
}

`year<-` <- function(x, value)
{
	.Call(`Cyear<-`, as.EDate(x), rep_len(as.integer(value), length(x)))
}

yday <- function(x)
{
	.Call(Cyday, as.EDate(x))
}

`yday<-` <- function(x, value)
{
	.Call(`Cyday<-`, as.EDate(x), rep_len(as.integer(value), length(x)))
}

month <- function(x)
{
	.Call(Cmonth, as.EDate(x))
}

`month<-` <- function(x, value)
{
	.Call(`Cmonth<-`, as.EDate(x), rep_len(as.integer(value), length(x)))
}

mday <- function(x)
{
	.Call(Cmday, as.EDate(x))
}

`mday<-` <- function(x, value)
{
	.Call(`Cmday<-`, as.EDate(x), rep_len(as.integer(value), length(x)))
}


year1 <- function(x)
{
	x <- unclass(as.IDate(x)) + 719163L ##/*0001-01-00*/
	guess <- x %/% 365L
	guess <- guess - (j00(guess) >= x)
	guess <- guess - (j00(guess) >= x)
	return(guess + 1L)
}

yday1 <- function(x)
{
	x <- unclass(as.IDate(x)) + 719163L ##/*0001-01-00*/
	guess <- x %/% 365L
	guess <- guess - (j00(guess) >= x)
	guess <- guess - (j00(guess) >= x)
	return(x - j00(guess))
}


## with -03-01 as start of year
j002 <- function(x)
{
	x * 365L + x %/% 4L - x %/% 100L + x %/% 400L
}

year2 <- function(x)
{
	x <- unclass(as.IDate(x)) + 719468L ##/*0000-03-01*/
	return(as.integer(trunc(x / 365.2425)))
}

yday2 <- function(x)
{
	x <- unclass(as.IDate(x)) + 719468L ##/*0000-03-01*/
	y <- as.integer(trunc(x / 365.2425))
	return(x - j002(y) + 1L)
}

month2 <- function(x)
{
## 31 30 31 30 31  31 30 31 30 31  31 28+
## cumsum(c(0,31,30,31,30,31, 31,30,31,30,31, 31)+31L)%/%64L is 0,1,2,3,...
	yd <- yday2(x) - 1L
	pent <- as.integer(trunc(yd / 153))
	yd <- (yd - 153L * pent) * 2L
	mo <- yd %/% 61L
	return(5L*pent+mo+1L)
}

mday2 <- function(x)
{
## 31 30 31 30 31  31 30 31 30 31  31 28+
## cumsum(c(0,31,30,31,30,31, 31,30,31,30,31, 31)+31L)%/%64L is 0,1,2,3,...
	yd <- yday2(x) - 1L
	pent <- as.integer(trunc(yd / 153))
	yd <- (yd - 153L * pent) * 2L
	md <- yd %% 61L %/% 2L
	return(md+1L)
}

as.FDate <- function(x, ...) UseMethod("as.FDate")

#FDate <- function(year, mon, day)
#{
#	4L + (year - 1L) * 391L + (mon - 1L) * 32 + ((mon-1)%/%6L) + ((mon-1)%/%3L) + day
#}

FDate <- function(year, mon, day)
{
	.Call(CFDate, as.integer(year), rep.int(as.integer(mon), length(year)), rep.int(as.integer(day), length(year)))
}

as.FDate.character <- function(x)
{
	.Call(Cas.FDate.character, as.character(x))
}

format.FDate <- function(x, ...)
{
	.Call(Cformat.FDate, x)
}

print.FDate <- function(x, ...)
{
	print(format(x), ...)
}

as.POSIXlt.FDate <- function(x)
{
	.Call(Cas.POSIXlt.FDate, x)
}
