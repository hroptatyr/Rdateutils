\name{qwcnt}
\alias{qwcnt}
\alias{qwcnt.FDate}
\title{
  Access date components
}
\description{
  Obtain the count of the weekday of a date where week counting is reset at the beginning of each quarter.
}
\usage{
qwcnt(x, \dots)
\method{qwcnt}{FDate}(x, \dots)
}
\arguments{
  \item{x}{vector of date objects.}
  \item{\dots}{further arguments, currently ignored}
}
\value{
  (Vector of) wcnt(s).
}
\keyword{classes}
\keyword{manip}
\seealso{\code{\link{as.wcnt}} \code{\link{qweek}} \code{\link{wcnt}} \code{\link{swcnt}} \code{\link{mwcnt}}}
\examples{
  qwcnt.FDate("2014-05-19")
  qwcnt.FDate("2014-12-19")
}
