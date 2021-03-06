\name{wday}
\alias{wday}
\alias{wday.FDate}
\alias{wday.wcnt}
\alias{wday<-}
\title{
  Access date components
}
\description{
  Obtain a numerical representation of the weekday of a date.

  For the assignment, set to the date whose weekday corresponds to value in the same
  ISO week as the original date.
}
\usage{
wday(x, \dots)
\method{wday}{FDate}(x, \dots)
\method{wday}{wcnt}(x, \dots)

wday(x, \dots) <- value
}
\arguments{
  \item{x}{vector of date or wcnt objects.}
  \item{\dots}{further arguments, currently ignored}
  \item{value}{integer value, range 1 (for Monday) to 7 (Sunday)}
}
\value{
  Integer value(s) representing the day of the week.  Range 1 to 7, where 1 is Monday
  and 7 is Sunday.
}
\keyword{classes}
\keyword{manip}
\seealso{\code{\link{week}} \code{\link{wcnt}} \code{\link{weekdays}}}
\examples{
  wday.FDate("2014-05-19")
  wday.wcnt("2R")

  as.FDate("2014-05-19") -> x
  wday(x) <- 3L
}
