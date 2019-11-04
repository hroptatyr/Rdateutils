\name{week}
\alias{week}
\alias{week.FDate}
\alias{week.ddur}
\alias{week.wcnt}
\alias{week<-}
\title{
  Access date components
}
\description{
  Obtain a numerical representation of the week of a date.

  For the assignment, set to the date whose weekday corresponds to value in the same
  ISO week as the original date.
}
\usage{
week(x, \dots)
\method{week}{FDate}(x, \dots)
\method{week}{ddur}(x, \dots)
\method{week}{wcnt}(x, \dots)

week(x, \dots) <- value
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
  week.FDate("2014-01-09")
  week.FDate("2014-05-19")
  week.ddur("P7W")

  as.FDate("2014-05-19") -> x
  week(x) <- 3L
}
