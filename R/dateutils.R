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
sweek <- function(x, ...) UseMethod("sweek")
qweek <- function(x, ...) UseMethod("qweek")
mweek <- function(x, ...) UseMethod("mweek")
wcnt <- function(x, ...) UseMethod("wcnt")
swcnt <- function(x, ...) UseMethod("swcnt")
qwcnt <- function(x, ...) UseMethod("qwcnt")
mwcnt <- function(x, ...) UseMethod("mwcnt")
dday <- function(x, ...) UseMethod("dday")

`year<-` <- function(x, ..., value) UseMethod("year<-")
`yday<-` <- function(x, ..., value) UseMethod("yday<-")
`sday<-` <- function(x, ..., value) UseMethod("sday<-")
`qday<-` <- function(x, ..., value) UseMethod("qday<-")
`month<-` <- function(x, ..., value) UseMethod("month<-")
`mday<-` <- function(x, ..., value) UseMethod("mday<-")
`week<-` <- function(x, ..., value) UseMethod("week<-")
`wday<-` <- function(x, ..., value) UseMethod("wday<-")
`dday<-` <- function(x, ..., value) UseMethod("dday<-")

nyday <- function(x, ...) UseMethod("nyday")
nsday <- function(x, ...) UseMethod("nsday")
nqday <- function(x, ...) UseMethod("nqday")
nmday <- function(x, ...) UseMethod("nmday")
nwday <- function(x, ...) UseMethod("nwday")

year.default <- function(x, ...)
{
	as.POSIXlt(x)$year + 1900L
}

yday.default <- function(x, ...)
{
	as.POSIXlt(x)$yday + 1L
}

semi.default <- function(x, ...)
{
	as.POSIXlt(x)$mon %/% 6L + 1L
}

quarter.default <- function(x, ...)
{
	as.POSIXlt(x)$mon %/% 3L + 1L
}

month.default <- function(x, ...)
{
	as.POSIXlt(x)$mon + 1L
}

mday.default <- function(x, ...)
{
	as.POSIXlt(x)$mday
}

week.default <- function(x, ...)
{
	z <- as.POSIXlt(x)
	(z$yday - (z$wday-1L)%%7L + 10L) %/% 7L
}

wday.default <- function(x, ...)
{
	as.POSIXlt(x)$wday
}

sweek.default <- function(x, ...)
{
	.Call(Csweek.FDate, as.FDate(x))
}

qweek.default <- function(x, ...)
{
	.Call(Cqweek.FDate, as.FDate(x))
}

mweek.default <- function(x, ...)
{
	.Call(Cmweek.FDate, as.FDate(x))
}

wcnt.default <- function(x, ...)
{
	z <- as.POSIXlt(x)
	z$yday %/% 7L + 1L
}

mwcnt.default <- function(x, ...)
{
	z <- as.POSIXlt(x)
	(z$mday - 1L) %/% 7L + 1L
}

nyday.default <- function(x, ...)
{
	.Call(Cnyday.FDate, as.FDate(x))
}

nsday.default <- function(x, ...)
{
	.Call(Cnsday.FDate, as.FDate(x))
}

nqday.default <- function(x, ...)
{
	.Call(Cnqday.FDate, as.FDate(x))
}

nmday.default <- function(x, ...)
{
	.Call(Cnmday.FDate, as.FDate(x))
}

nwday.default <- function(x, ...)
{
	7L
}


as.FDate <- function(x, ...) UseMethod("as.FDate")
is.FDate <- function(x)
{
	inherits(x, "FDate")
}

as.FDate.character <- function(x, ...)
{
	.Call(Cas.FDate.character, x)
}

as.FDate.factor <- function(x, ...)
{
	.Call(Cas.FDate.factor, x)
}

as.FDate.FDate <- function(x, ...)
{
	return(x)
}

as.FDate.integer <- function(x, ...)
{
	.Call(Cas.FDate.integer, x);
}

as.FDate.Date <- as.FDate.IDate <- function(x, ...)
{
	.Call(Cas.FDate.IDate, as.integer(unclass(x)))
}

as.FDate.default <- function(x, ...)
{
## go through Date
	.Call(Cas.FDate.IDate, as.integer(unclass(as.Date(x, ...))))
}

as.character.FDate <- format.FDate <- function(x, ...)
{
	.Call(Cformat.FDate, x)
}

print.FDate <- function(x, ...)
{
	print(format.FDate(x), ...)
}

as.Date.FDate <- function(x, ...)
{
	.Call(Cas.IDate.FDate, x)
}

as.POSIXlt.FDate <- function(x, ...)
{
	.Call(Cas.POSIXlt.FDate, x)
}

c.FDate <- rev.FDate <- cut.FDate <- mean.FDate <-
rep.FDate <- split.FDate <- unique.FDate <-
min.FDate <- max.FDate <- "[.FDate" <- "[[.FDate" <- function(x, ...)
{
	x <- NextMethod()
	class(x) <- c("FDate",".duo")
	x
}

trunc.FDate <- function(x, units=c("days", "weeks", "months", "quarters", "semis", "years"), ...)
{
	units <- match.arg(units)
	x <- as.FDate(x)
	switch(units,
	days=x,
	weeks=x - (wday.FDate(x) - 1L),
	months=.Call(Ctrunc.FDate.month, x),
	quarters=.Call(Ctrunc.FDate.quarter, x),
	semis=.Call(Ctrunc.FDate.semi, x),
	years=.Call(Ctrunc.FDate.year, x),
	rep.int(as.FDate(NA_integer_), length(x)))
}

seq.FDate <- function(from, till, by, from.last=F, ...)
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
year.FDate <- function(x, ...)
{
	.Call(Cyear.FDate, as.FDate(x))
}

yday.FDate <- function(x, ...)
{
	.Call(Cyday.FDate, as.FDate(x))
}

semi.FDate <- function(x, ...)
{
	.Call(Csemi.FDate, as.FDate(x))
}

sday.FDate <- function(x, ...)
{
	.Call(Csday.FDate, as.FDate(x))
}

quarter.FDate <- function(x, ...)
{
	.Call(Cquarter.FDate, as.FDate(x))
}

qday.FDate <- function(x, ...)
{
	.Call(Cqday.FDate, as.FDate(x))
}

month.FDate <- function(x, ...)
{
	.Call(Cmonth.FDate, as.FDate(x))
}

mday.FDate <- function(x, ...)
{
	.Call(Cmday.FDate, as.FDate(x))
}

week.FDate <- function(x, ...)
{
	.Call(Cweek.FDate, as.FDate(x))
}

wday.FDate <- function(x, ...)
{
	.Call(Cwday.FDate, as.FDate(x))
}

sweek.FDate <- function(x, ...)
{
	.Call(Csweek.FDate, as.FDate(x))
}

qweek.FDate <- function(x, ...)
{
	.Call(Cqweek.FDate, as.FDate(x))
}

mweek.FDate <- function(x, ...)
{
	.Call(Cmweek.FDate, as.FDate(x))
}

wcnt.FDate <- function(x, ...)
{
	.Call(Cwcnt.FDate, as.FDate(x))
}

swcnt.FDate <- function(x, ...)
{
	.Call(Cswcnt.FDate, as.FDate(x))
}

qwcnt.FDate <- function(x, ...)
{
	.Call(Cqwcnt.FDate, as.FDate(x))
}

mwcnt.FDate <- function(x, ...)
{
	.Call(Cmwcnt.FDate, as.FDate(x))
}

`year<-.FDate` <- function(x, ..., value)
{
	.Call(`Cyear<-.FDate`, as.FDate(x), rep_len(as.integer(value), length(x)))
}

`yday<-.FDate` <- function(x, ..., value)
{
	.Call(`Cyday<-.FDate`, as.FDate(x), rep_len(as.integer(value), length(x)))
}

`sday<-.FDate` <- function(x, ..., value)
{
	.Call(`Csday<-.FDate`, as.FDate(x), rep_len(as.integer(value), length(x)))
}

`qday<-.FDate` <- function(x, ..., value)
{
	.Call(`Cqday<-.FDate`, as.FDate(x), rep_len(as.integer(value), length(x)))
}

`month<-.FDate` <- function(x, ..., value)
{
	.Call(`Cmonth<-.FDate`, as.FDate(x), rep_len(as.integer(value), length(x)))
}

`mday<-.FDate` <- function(x, ..., value)
{
	.Call(`Cmday<-.FDate`, as.FDate(x), rep_len(as.integer(value), length(x)))
}

`week<-.FDate` <- function(x, ..., value)
{
	.Call(`Cweek<-.FDate`, as.FDate(x), rep_len(as.integer(value), length(x)))
}

`wday<-.FDate` <- function(x, ..., value)
{
	.Call(`Cwday<-.FDate`, as.FDate(x), rep_len(as.integer(value), length(x)))
}


## integrate with base
weekdays.FDate <- function(x, abbreviate=FALSE)
{
	wd <- list(
		c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
		c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"),
		c("S","M","T","W","R","F","A")
	)
	wd[[abbreviate+1L]][wday(x) + 1L]
}

months.FDate <- function(x, abbreviate=FALSE)
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

as.ddur.character <- function(x, ...)
{
	.Call(Cas.ddur.character, x)
}

as.ddur.factor <- function(x, ...)
{
	.Call(Cas.ddur.factor, x)
}

as.ddur.numeric <- function(x, ...)
{
	.Call(Cas.ddur.numeric, x)
}

as.ddur.wcnt <- function(x, ...)
{
	.Call(Cas.ddur.wcnt, x)
}

as.ddur.logical <- function(x, ...)
{
	.Call(Cas.ddur.numeric, NA_integer_)
}

as.character.ddur <- format.ddur <- function(x, ...)
{
	.Call(Cformat.ddur, x)
}

print.ddur <- function(x, ...)
{
	print(format.ddur(x), ...)
}

c.ddur <- rev.ddur <- rep.ddur <-
round.ddur <- unique.ddur <- "[.ddur" <- "[[.ddur" <- function(x, ...)
{
	x <- NextMethod()
	class(x) <- c("ddur",".duo")
	x
}

seq.ddur <- function(from, till, by, from.last=F, ...)
{
	if (length(from) != 1L) {
		stop("FROM must be of length 1")
	} else if (!is.finite(from <- as.ddur(from))) {
		stop("FROM must be finite")
	}
	if (length(till) != 1L) {
		stop("TILL must be of length 1")
	} else if (!is.finite(till <- as.ddur(till))) {
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
		x <- .Call(Cseq.ddur, till, from, -by)
		return(rev(x))
	}
	.Call(Cseq.ddur, from, till, by)
}

ddur <- function(x, y, day.count=c("act", "30", "30A", "30E", "30I", "30U", "bd", "28"))
{
## actual (day) duration from x to y
	if (missing(y)) {
		return(as.ddur(x))
	}
	day.count <- match.arg(day.count)

	x <- as.FDate(x)
	if (all(is.na(z <- as.FDate(y)))) {
		y <- as.ddur(y)
	} else {
		y <- z
	}
	if (inherits(y, "ddur")) {
		y <- x + y
	}

	if (day.count == "act") {
		md <- .Call(Cddur.FDate, x, rep.int(y, length(x)))
	} else {
		y <- rep(y, length(x))
		if (day.count == "30A") {
			mday(x[mday(x) > 30L]) <- 30L
			mday(y[mday(x) >= 30L & mday(y) > 30L]) <- 30L
		} else if (day.count == "30U") {
			mday(x[mday(x) == nmday(x)]) <- 30L
			mday(y[mday(x) >= 30L & mday(y) == nmday(x)]) <- 30L
		} else if (day.count == "30E") {
			mday(x[mday(x) > 30L]) <- 30L
			mday(y[mday(x) > 30L]) <- 30L
		}
		md <- .Call(`C-.FDate`, y, x)
		md <- md + month(md)*30
		month(md) <- 0
	}
	md
}

ydur <- function(x, y, day.count=c("act/act", "30/360", "30A/360", "30E/360", "30I/360", "30U/360", "act/360", "act/365", "act/365A", "bd/252", "28/360"))
{
	day.count <- match.arg(day.count)

	x <- as.FDate(x)
	if (all(is.na(z <- as.FDate(y)))) {
		y <- as.ddur(y)
	} else {
		y <- z
	}
	if (inherits(y, "ddur")) {
		y <- x + y
	}

	if (switch(day.count,
		`act/act`=TRUE, `act/360`=TRUE,
		`act/365`=TRUE, `act/365A`=TRUE,
		FALSE)) {
		act <- dday(.Call(Cddur.FDate, x, rep.int(y, length(x))))
		switch(day.count,
		`act/act`=act/nyday(x),
		`act/365A`=act/nyday(x),
		`act/360`=act/360,
		`act/365`=act/365)
	} else {
		y <- rep(y, length(x))
		if (day.count == "30A/360") {
			mday(x[mday(x) > 30L]) <- 30L
			mday(y[mday(x) >= 30L & mday(y) > 30L]) <- 30L
		} else if (day.count == "30U/360") {
			mday(x[mday(x) == nmday(x)]) <- 30L
			mday(y[mday(x) >= 30L & mday(y) == nmday(x)]) <- 30L
		} else if (day.count == "30E/360") {
			mday(x[mday(x) > 30L]) <- 30L
			mday(y[mday(x) > 30L]) <- 30L
		}
		md <- .Call(`C-.FDate`, y, x)
		(month(md)*30+dday(md))/360
	}
}

## accessors
year.ddur <- function(x, ...)
{
	.Call(Cyear.ddur, x)
}

semi.ddur <- function(x, ...)
{
	.Call(Csemi.ddur, x)
}

quarter.ddur <- function(x, ...)
{
	.Call(Cquarter.ddur, x)
}

month.ddur <- function(x, ...)
{
	.Call(Cmonth.ddur, x)
}

week.ddur <- function(x, ...)
{
	.Call(Cweek.ddur, x)
}

dday.ddur <- function(x, ...)
{
	.Call(Cdday.ddur, x)
}

`year<-.ddur` <- function(x, ..., value)
{
	.Call(`Cyear<-.ddur`, x, rep_len(as.integer(value), length(x)))
}

`month<-.ddur` <- function(x, ..., value)
{
	.Call(`Cmonth<-.ddur`, x, rep_len(as.integer(value), length(x)))
}

`week<-.ddur` <- function(x, ..., value)
{
	.Call(`Cweek<-.ddur`, x, rep_len(as.integer(value), length(x)))
}

`dday<-.ddur` <- function(x, ..., value)
{
	.Call(`Cdday<-.ddur`, x, rep_len(as.integer(value), length(x)))
}


## multidispatch stuff
`+..duo` <- function(x, y)
{
## pretend it's a multidispatch
	if (nargs() == 1L) {
		return(x)
	}
	if (inherits(x, "FDate")) {
		return(.Call(`C+.FDate`, x, rep.int(as.ddur(y), length(x))))
	}
	if (inherits(x, "wcnt")) {
		return(.Call(`C+.wcnt`, x, rep.int(as.ddur(y), length(x))))
	}
	if (inherits(x, "ddur") || inherits(y, "ddur")) {
		return(.Call(`C+.ddur`, as.ddur(x), rep.int(as.ddur(y), length(x))))
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
	if (inherits(x, "FDate")) {
		if (is.ddur(y)) {
			;
		} else if (is.FDate(z <- y) || !all(is.na(z <- as.FDate(y)))) {
			return(.Call(`C-.FDate`, x, rep.int(z, length(x))))
		}
		z <- .Call(Cneg.ddur, as.ddur(y))
		return(.Call(`C+.FDate`, x, rep.int(z, length(x))))
	}
	if (inherits(x, "ddur") || inherits(y, "ddur")) {
		return(.Call(`C+.ddur`, as.ddur(x), .Call(Cneg.ddur, as.ddur(y))))
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


## convenience
`%before%` <- function(x, y) UseMethod("%before%")
`%before|on%` <- function(x, y) UseMethod("%before|on%")
`%after%` <- function(x, y) UseMethod("%after%")
`%after|on%` <- function(x, y) UseMethod("%after|on%")
`older.than` <- function(x, y, today, ...) UseMethod("older.than")
`newer.than` <- function(x, y, today, ...) UseMethod("newer.than")
`%older.than%` <- function(x, y) UseMethod("%older.than%")
`%newer.than%` <- function(x, y) UseMethod("%newer.than%")

`<..duo` <- `%before%..duo` <- function(x, y)
{
## use FDate as super-type as they can hold more dates
	if (inherits(x, "ddur")) {
		return(.Call(`C<.ddur`, x, rep.int(as.ddur(y), length(x))))
	}
	unclass(as.FDate(x)) < unclass(as.FDate(y))
}

`<=..duo` <- `%before|on%..duo` <- function(x, y)
{
## use FDate as super-type as they can hold more dates
	if (inherits(x, "ddur")) {
		return(.Call(`C<=.ddur`, x, rep.int(as.ddur(y), length(x))))
	}
	unclass(as.FDate(x)) <= unclass(as.FDate(y))
}

`>..duo` <- `%after%..duo` <- function(x, y)
{
## use FDate as super-type as they can hold more dates
	if (inherits(x, "ddur")) {
		return(.Call(`C>.ddur`, x, rep.int(as.ddur(y), length(x))))
	}
	unclass(as.FDate(x)) > unclass(as.FDate(y))
}

`>=..duo` <- `%after|on%..duo` <- function(x, y)
{
## use FDate as super-type as they can hold more dates
	if (inherits(x, "ddur")) {
		return(.Call(`C>=.ddur`, x, rep.int(as.ddur(y), length(x))))
	}
	unclass(as.FDate(x)) >= unclass(as.FDate(y))
}

`==..duo` <- function(x, y)
{
## use FDate as super-type as they can hold more dates
	if (inherits(x, "ddur") || inherits(y, "ddur")) {
		;
	} else {
		x <- as.FDate(x)
		y <- as.FDate(y)
	}
	unclass(x) == unclass(y)
}

`!=..duo` <- function(x, y)
{
## use FDate as super-type as they can hold more dates
	if (inherits(x, "ddur") || inherits(y, "ddur")) {
		;
	} else {
		x <- as.FDate(x)
		y <- as.FDate(y)
	}
	unclass(x) != unclass(y)
}

`older.than..duo` <- function(x, y, today=Sys.Date(), ...)
{
	y <- as.ddur(y)
	if (is.FDate(x)) {
		return(unclass(.Call(`C+.FDate`, x, y)) < unclass(as.FDate(today)))
	}
	return(.Call(`C<.ddur`, as.ddur(x), y))
}

`%older.than%..duo` <- function(x, y)
{
	y <- as.ddur(y)
	if (is.FDate(x)) {
		return(unclass(.Call(`C+.FDate`, x, y)) < unclass(as.FDate(Sys.Date())))
	}
	return(.Call(`C<.ddur`, as.ddur(x), y))
}

`newer.than..duo` <- function(x, y, today=Sys.Date(), ...)
{
	y <- rep.int(as.ddur(y), length(x))
	if (is.FDate(x)) {
		return(unclass(.Call(`C+.FDate`, x, y)) > unclass(as.FDate(today)))
	}
	return(.Call(`C>.ddur`, as.ddur(x), y))
}

`%newer.than%..duo` <- function(x, y)
{
	y <- rep.int(as.ddur(y), length(x))
	if (is.FDate(x)) {
		return(unclass(.Call(`C+.FDate`, x, y)) > unclass(as.FDate(Sys.Date())))
	}
	return(.Call(`C>.ddur`, as.ddur(x), y))
}

oldest <- function(dates, span, which=FALSE)
{
## invariant: oldest(X, SPAN) + newest(X, -SPAN) = X
	if (length(span) != 1L) {
		stop("SPAN must be of length 1")
	}
	span <- as.ddur(span)
	pivot <- if (span < 0) {
		max(dates)+1L
	} else if (span > 0) {
		min(dates)
	} else {
		stop("SPAN must not be nought")
	}
	r <- dates %before% (pivot+span)
	if (which) {
		return(which(r))
	}
	r
}

newest <- function(dates, span, which=FALSE)
{
## invariant: newest(X, SPAN) + oldest(X, -SPAN) = X
	if (length(span) != 1L) {
		stop("SPAN must be of length 1")
	}
	span <- .Call(Cneg.ddur, as.ddur(span))
	pivot <- if (span < 0) {
		max(dates)
	} else if (span > 0) {
		min(dates)-1L
	} else {
		stop("SPAN must not be nought")
	}
	r <- dates %after% (pivot+span)
	if (which) {
		return(which(r))
	}
	r
}


as.wcnt <- function(x, ...) UseMethod("as.wcnt")
is.wcnt <- function(x)
{
	inherits(x, "wcnt")
}

as.wcnt.wcnt <- function(x, ...)
{
	return(x)
}

as.wcnt.character <- function(x, ...)
{
	.Call(Cas.wcnt.character, x)
}

as.wcnt.factor <- function(x, ...)
{
	.Call(Cas.wcnt.factor, x)
}

as.wcnt.numeric <- function(x, wd, ...)
{
	x <- as.integer(x) * 8L + as.integer(wd)
	class(x) <- c("wcnt",".duo")
	x
}

as.character.wcnt <- format.wcnt <- function(x, ...)
{
	.Call(Cformat.wcnt, x)
}

print.wcnt <- function(x, ...)
{
	print(format.wcnt(x), ...)
}

## accessors
week.wcnt <- function(x, ...)
{
	.Call(Cweek.wcnt, as.wcnt(x, ...))
}

wday.wcnt <- function(x, ...)
{
	.Call(Cwday.wcnt, as.wcnt(x, ...))
}

`week<-.wcnt` <- function(x, ..., value)
{
	.Call(`Cweek<-.wcnt`, x, rep_len(value, length(x)))
}

`wday<-.wcnt` <- function(x, ..., value)
{
	.Call(`Cwday<-.wcnt`, x, rep_len(value, length(x)))
}

c.wcnt <- rev.wcnt <- cut.wcnt <- rep.wcnt <-
split.wcnt <- unique.wcnt <- min.wcnt <- max.wcnt <- function(x, ...)
{
	x <- NextMethod()
	class(x) <- c("wcnt",".duo")
	x
}
