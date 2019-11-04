\name{mweek}
\alias{mweek}
\alias{mweek.FDate}
\title{
  Access date components
}
\description{
  Obtain the week of a date where week counting is reset at the beginning of the month.

  Like ISO weeks within the year the first week of a month is the week that contains
  the 4th of the month.
}
\usage{
mweek(x, \dots)
\method{mweek}{FDate}(x, \dots)
}
\arguments{
  \item{x}{vector of date objects.}
  \item{\dots}{further arguments, currently ignored}
}
\value{
  Integer value(s) representing the week within the month.  Range 0 to 5.
}
\keyword{classes}
\keyword{manip}
\seealso{\code{\link{week}} \code{\link{sweek}} \code{\link{qweek}}}
\examples{
  mweek.FDate("2014-05-19")
}
