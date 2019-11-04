\name{sweek}
\alias{sweek}
\alias{sweek.FDate}
\title{
  Access date components
}
\description{
  Obtain the week of a date where week counting is reset at the beginning of each semester.

  Like ISO weeks within the year the first week of a semester is the week that contains
  the 4th of the semester.
}
\usage{
sweek(x, \dots)
\method{sweek}{FDate}(x, \dots)
}
\arguments{
  \item{x}{vector of date objects.}
  \item{\dots}{further arguments, currently ignored}
}
\value{
  Integer value(s) representing the week within the semester.  Range 0 to 27.
}
\keyword{classes}
\keyword{manip}
\seealso{\code{\link{week}} \code{\link{qweek}} \code{\link{mweek}}}
\examples{
  sweek.FDate("2014-05-19")
}
