\name{wcnt}
\alias{wcnt}
\alias{wcnt.FDate}
\title{
  Access date components
}
\description{
  Obtain the count of the weekday of a date where week counting is reset at the beginning of the year.
}
\usage{
wcnt(x, \dots)
\method{wcnt}{FDate}(x, \dots)
}
\arguments{
  \item{x}{vector of date objects.}
  \item{\dots}{further arguments, currently ignored}
}
\value{
  Integer value(s) representing the count of the weekday of date within the year.
  Range 1 to 53.
}
\keyword{classes}
\keyword{manip}
\seealso{\code{\link{week}} \code{\link{mwcnt}}}
\examples{
  wcnt.FDate("2014-05-19")
}
