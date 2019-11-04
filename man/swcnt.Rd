\name{swcnt}
\alias{swcnt}
\alias{swcnt.FDate}
\title{
  Access date components
}
\description{
  Obtain the count of the weekday of a date where week counting is reset at the beginning of each semester.
}
\usage{
swcnt(x, \dots)
\method{swcnt}{FDate}(x, \dots)
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
\seealso{\code{\link{as.wcnt}} \code{\link{sweek}} \code{\link{wcnt}} \code{\link{qwcnt}} \code{\link{mwcnt}}}
\examples{
  swcnt.FDate("2014-05-19")
  swcnt.FDate("2014-12-19")
}
