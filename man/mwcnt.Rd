\name{mwcnt}
\alias{mwcnt}
\alias{mwcnt.FDate}
\title{
  Access date components
}
\description{
  Obtain the count of the weekday of a date where week counting is reset at the beginning of the month.
}
\usage{
mwcnt(x, \dots)
\method{mwcnt}{FDate}(x, \dots)
}
\arguments{
  \item{x}{vector of date objects.}
  \item{\dots}{further arguments, currently ignored}
}
\value{
  Integer value(s) representing the count of the weekday of date within the month.
  Range 1 to 5.
}
\keyword{classes}
\keyword{manip}
\seealso{\code{\link{mweek}} \code{\link{wcnt}}}
\examples{
  mwcnt.FDate("2014-05-19")
}
