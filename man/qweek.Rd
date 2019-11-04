\name{qweek}
\alias{qweek}
\alias{qweek.FDate}
\title{
  Access date components
}
\description{
  Obtain the week of a date where week counting is reset at the beginning of each quarter.

  Like ISO weeks within the year the first week of a quarter is the week that contains
  the 4th of the quarter.
}
\usage{
qweek(x, \dots)
\method{qweek}{FDate}(x, \dots)
}
\arguments{
  \item{x}{vector of date objects.}
  \item{\dots}{further arguments, currently ignored}
}
\value{
  Integer value(s) representing the week within the quarter.  Range 0 to 14.
}
\keyword{classes}
\keyword{manip}
\seealso{\code{\link{week}} \code{\link{sweek}} \code{\link{mweek}}}
\examples{
  qweek.FDate("2014-05-19")
}
