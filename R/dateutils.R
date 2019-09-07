year <- function(x)
{
	x <- unclass(as.IDate(x)) + 719468L ##/*0001-03-00*/
	return(.Call(Cyear, x))
}

yday <- function(x)
{
	x <- unclass(as.IDate(x)) + 719468L ##/*0001-03-00*/
	return(.Call(Cyday, x))
}

month <- function(x)
{
	x <- unclass(as.IDate(x)) + 719468L ##/*0001-03-00*/
	return(.Call(Cmonth, x))
}

mday <- function(x)
{
	x <- unclass(as.IDate(x)) + 719468L ##/*0001-03-00*/
	return(.Call(Cmday, x))
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
