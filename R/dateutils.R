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
dday <- function(x, ...) UseMethod("dday")

year.default <- function(x)
{
	as.POSIXlt(x)$year + 1900L
}

yday.default <- function(x)
{
	as.POSIXlt(x)$yday + 1L
}

semi.default <- function(x)
{
	as.POSIXlt(x)$mon %/% 6L + 1L
}

quarter.default <- function(x)
{
	as.POSIXlt(x)$mon %/% 3L + 1L
}

month.default <- function(x)
{
	as.POSIXlt(x)$mon + 1L
}

mday.default <- function(x)
{
	as.POSIXlt(x)$mday
}

week.default <- function(x)
{
	z <- as.POSIXlt(x)
	(z$yday - (z$wday-1L)%%7L + 10L) %/% 7L
}

wday.default <- function(x)
{
	as.POSIXlt(x)$wday
}


as.EDate <- function(x, ...) UseMethod("as.EDate")
is.EDate <- function(x)
{
	inherits(x, "EDate")
}

as.EDate.default <- function(x, ..., tz=attr(x, "tzone"))
{
	as.EDate(as.Date(x, tz=if(!is.null(tz)) tz else "UTC", ...))
}

as.EDate.character <- function(x)
{
	.Call(Cas.EDate.character, x)
}

as.EDate.Date <- as.EDate.IDate <- function(x, ...)
{
	x <- as.integer(x) + 719469L ##/*0000-03-00*/
	class(x) <- c("EDate",".duo")
	return(x)
}

as.EDate.POSIXct <- function(x, tz=attr(x, "tzone"), ...)
{
	if (is.null(tz) || tz == "UTC") {
		x <- as.integer(x / 86400)
		class(x) <- c("EDate",".duo")
		return(x)
	}
	return(as.EDate(as.Date(x, tz=tz, ...)))
}

as.EDate.EDate <- function(x, ...)
{
	return(x)
}

as.EDate.integer <- function(x)
{
	.Call(Cas.EDate.integer, x);
}

as.Date.EDate <- function(x, ...)
{
	x <- as.numeric(x) - 719469L
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

c.EDate <- rev.EDate <- cut.EDate <- mean.EDate <-
cut.EDate <- mean.EDate <- rep.EDate <-
round.EDate <- split.EDate <- unique.EDate <-
min.EDate <- max.EDate <- "[.EDate" <- function(x, ...)
{
	x <- NextMethod()
	class(x) <- c("EDate",".duo")
	return(x)
}

seq.EDate <- function(from, till, by=ddur(1L), from.last=F)
{
## follow dateseq(1) semantics
	if (length(from) != 1L) {
		stop("FROM must be of length 1")
	} else if (!is.finite(from <- as.EDate(from))) {
		stop("FROM must be finite")
	}
	if (length(till) != 1L) {
		stop("TILL must be of length 1")
	} else if (!is.finite(till <- as.EDate(till))) {
		stop("TILL must be finite")
	}
	if (length(by) != 1L) {
		stop("BY must be of length 1")
	} else if (is.na(by <- as.ddur(by))) {
		stop("BY must be non-NA")
	}
	if (from == till) {
		## regardless of BY
		return(from)
	} else if (!by) {
		stop("BY must be non-zero")
	}
	if (from.last) {
		x <- .Call(Cseq.EDate, till, from, -by)
		return(rev(x))
	}
	.Call(Cseq.EDate, from, till, by)
}

## accessors
year.EDate <- function(x)
{
	.Call(Cyear.EDate, as.EDate(x))
}

`year<-` <- function(x, value)
{
	.Call(`Cyear<-`, as.EDate(x), rep_len(as.integer(value), length(x)))
}

yday.EDate <- function(x)
{
	.Call(Cyday.EDate, as.EDate(x))
}

`yday<-` <- function(x, value)
{
	.Call(`Cyday<-`, as.EDate(x), rep_len(as.integer(value), length(x)))
}

semi.EDate <- function(x)
{
	.Call(Csemi.EDate, as.EDate(x))
}

sday.EDate <- function(x)
{
	.Call(Csday.EDate, as.EDate(x))
}

quarter.EDate <- function(x)
{
	.Call(Cquarter.EDate, as.EDate(x))
}

qday.EDate <- function(x)
{
	.Call(Cqday.EDate, as.EDate(x))
}

month.EDate <- function(x)
{
	.Call(Cmonth.EDate, as.EDate(x))
}

`month<-` <- function(x, value)
{
	.Call(`Cmonth<-`, as.EDate(x), rep_len(as.integer(value), length(x)))
}

mday.EDate <- function(x)
{
	.Call(Cmday.EDate, as.EDate(x))
}

`mday<-` <- function(x, value)
{
	.Call(`Cmday<-`, as.EDate(x), rep_len(as.integer(value), length(x)))
}

week.EDate <- function(x)
{
	.Call(Cweek.EDate, as.EDate(x))
}

wday.EDate <- function(x)
{
	.Call(Cwday.EDate, as.EDate(x))
}


as.FDate <- function(x, ...) UseMethod("as.FDate")
is.FDate <- function(x)
{
	inherits(x, "FDate")
}

as.FDate.character <- function(x)
{
	.Call(Cas.FDate.character, x)
}

as.FDate.FDate <- function(x, ...)
{
	return(x)
}

as.FDate.integer <- function(x)
{
	.Call(Cas.FDate.integer, x);
}

as.FDate.EDate <- function(x)
{
	.Call(Cas.FDate.EDate, x)
}

as.FDate.default <- function(x)
{
## go through EDates
	.Call(Cas.FDate.EDate, as.EDate(x))
}

format.FDate <- function(x, ...)
{
	.Call(Cformat.FDate, x)
}

print.FDate <- function(x, ...)
{
	print(format.FDate(x), ...)
}

as.EDate.FDate <- function(x)
{
	.Call(Cas.EDate.FDate, x)
}

as.POSIXlt.FDate <- function(x)
{
	.Call(Cas.POSIXlt.FDate, x)
}

c.FDate <- rev.FDate <- cut.FDate <- mean.FDate <-
cut.FDate <- mean.FDate <- rep.FDate <-
round.FDate <- split.FDate <- unique.FDate <-
min.FDate <- max.FDate <- "[.FDate" <- function(x, ...)
{
	x <- NextMethod()
	class(x) <- c("FDate",".duo")
	x
}

seq.FDate <- function(from, till, by, from.last=F)
{
## follow dateseq(1) semantics
	if (length(from) != 1L) {
		stop("FROM must be of length 1")
	} else if (!is.finite(from <- as.FDate(from))) {
		stop("FROM must be finite")
	}
	if (length(till) != 1L) {
		stop("TILL must be of length 1")
	} else if (!is.finite(till <- as.FDate(till))) {
		stop("TILL must be finite")
	}
	if (from == till) {
		## regardless of BY
		return(from)
	} else if (missing(by)) {
		by <- ddur(0L)
	} else if (length(by) != 1L) {
		stop("BY must be of length 1")
	} else if (is.na(by <- as.ddur(by))) {
		stop("BY must be non-NA")
	} else if (!by) {
		stop("BY must be non-zero")
	}
	if (from.last) {
		x <- .Call(Cseq.FDate, till, from, -by)
		return(rev(x))
	}
	.Call(Cseq.FDate, from, till, by)
}

## accessors
year.FDate <- function(x)
{
	.Call(Cyear.FDate, as.FDate(x))
}

yday.FDate <- function(x)
{
	.Call(Cyday.FDate, as.FDate(x))
}

semi.FDate <- function(x)
{
	.Call(Csemi.FDate, as.FDate(x))
}

sday.FDate <- function(x)
{
	.Call(Csday.FDate, as.FDate(x))
}

quarter.FDate <- function(x)
{
	.Call(Cquarter.FDate, as.FDate(x))
}

qday.FDate <- function(x)
{
	.Call(Cqday.FDate, as.FDate(x))
}

month.FDate <- function(x)
{
	.Call(Cmonth.FDate, as.FDate(x))
}

mday.FDate <- function(x)
{
	.Call(Cmday.FDate, as.FDate(x))
}

week.FDate <- function(x)
{
	.Call(Cweek.FDate, as.FDate(x))
}

wday.FDate <- function(x)
{
	.Call(Cwday.FDate, as.FDate(x))
}


## integrate with base
weekdays.FDate <- weekdays.EDate <- function(x, abbreviate=FALSE)
{
	wd <- list(
		c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
		c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"),
		c("S","M","T","W","R","F","A")
	)
	wd[[abbreviate+1L]][wday(x) + 1L]
}

months.FDate <- months.EDate <- function(x, abbreviate=FALSE)
{
	ms <- list(
		c("January", "February", "March", "April", "May", "June",
			"July", "August", "September", "October", "November", "December"),
		c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
			"Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
		c("F","G","H","J","K","M","N","Q","U","V","X","Z")
	)
	ms[[abbreviate+1L]][month(x)]
}


as.ddur <- function(x, ...) UseMethod("as.ddur")
is.ddur <- function(x)
{
	inherits(x, "ddur")
}

as.ddur.ddur <- function(x, ...)
{
	return(x)
}

as.ddur.character <- function(x)
{
	.Call(Cas.ddur.character, x)
}

as.ddur.numeric <- function(x)
{
	.Call(Cas.ddur.numeric, x)
}

format.ddur <- function(x, ...)
{
	.Call(Cformat.ddur, x)
}

print.ddur <- function(x, ...)
{
	print(format.ddur(x), ...)
}

c.ddur <- rev.ddur <- rep.ddur <-
round.ddur <- unique.ddur <- "[.ddur" <- function(x, ...)
{
	x <- NextMethod()
	class(x) <- c("ddur",".duo")
	x
}

ddur <- function(x, y)
{
## actual (day) duration from x to y
	if (missing(y)) {
		return(as.ddur(x))
	}
	if (inherits(x, "EDate")) {
		return(.Call(Cddur.EDate, x, rep.int(as.EDate(y), length(x))))
	}
	if (inherits(x, "FDate")) {
		return(.Call(Cddur.FDate, x, rep.int(as.FDate(y), length(x))))
	}
	stop("no method found to obtain duration between ",class(x)," and ",class(y))
}

## accessors
year.ddur <- function(x)
{
	.Call(Cyear.ddur, x)
}

semi.ddur <- function(x)
{
	.Call(Csemi.ddur, x)
}

quarter.ddur <- function(x)
{
	.Call(Cquarter.ddur, x)
}

month.ddur <- function(x)
{
	.Call(Cmonth.ddur, x)
}

week.ddur <- function(x)
{
	.Call(Cweek.ddur, x)
}

dday.ddur <- function(x)
{
	.Call(Cdday.ddur, x)
}


## multidispatch stuff
`+..duo` <- function(x, y)
{
## pretend it's a multidispatch
	if (nargs() == 1L) {
		return(x)
	}
	if (inherits(x, "EDate")) {
		return(.Call(`C+.EDate`, x, rep.int(as.ddur(y), length(x))))
	}
	if (inherits(x, "FDate")) {
		return(.Call(`C+.FDate`, x, rep.int(as.ddur(y), length(x))))
	}
	if (inherits(x, "ddur") || inherits(y, "ddur")) {
		if (inherits(x, "ddur") && inherits(y, "ddur")) {
			;
		} else if (inherits(x, "ddur")) {
			y <- as.ddur(y)
		} else if (inherits(y, "ddur")) {
			x <- as.ddur(x)
		}
		return(.Call(`C+.ddur`, x, y))
	}
	stop("no method found to add ",class(y)," to ",class(x))
}

`-..duo` <- function(x, y)
{
	if (nargs() == 1L && inherits(x, "ddur")) {
		return(.Call(Cneg.ddur, x))
	} else if (nargs() == 1L) {
		stop("unary minus is undefined for ",class(x))
	}
	if (inherits(x, "EDate")) {
		if (!all(is.na(z <- as.EDate(y)))) {
			return(.Call(`C-.EDate`, x, rep.int(z, length(x))))
		}
		z <- .Call(Cneg.ddur, as.ddur(y))
		return(.Call(`C+.EDate`, x, rep.int(z, length(x))))
	}
	if (inherits(x, "FDate")) {
		if (!all(is.na(z <- as.FDate(y)))) {
			return(.Call(`C-.FDate`, x, rep.int(z, length(x))))
		}
		z <- .Call(Cneg.ddur, as.ddur(y))
		return(.Call(`C+.FDate`, x, rep.int(z, length(x))))
	}
	if (inherits(x, "ddur") || inherits(y, "ddur")) {
		if (inherits(x, "ddur") && inherits(y, "ddur")) {
			;
		} else if (inherits(x, "ddur")) {
			y <- as.ddur(y)
		} else if (inherits(y, "ddur")) {
			x <- as.ddur(x)
		}
		y <- .Call(Cneg.ddur, y)
		return(.Call(`C+.ddur`, x, y))
	}
	stop("no method found to subtract ",class(y)," from ",class(x))
}

`*.ddur` <- function(x, y)
{
	.Call(`C*.ddur`, x, rep.int(as.numeric(y), length(x)))
}

`/.ddur` <- function(x, y)
{
	.Call(`C/.ddur`, x, rep.int(as.numeric(y), length(x)))
}

`%%.ddur` <- function(x, y)
{
	.Call(`C%.ddur`, x, rep.int(as.numeric(y), length(x)))
}
