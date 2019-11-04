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
  (Vector of) wcnt(s).
}
\keyword{classes}
\keyword{manip}
\seealso{\code{\link{as.wcnt}} \code{\link{mweek}} \code{\link{wcnt}} \code{\link{swcnt}} \code{\link{qwcnt}}}
\examples{
  mwcnt.FDate("2014-05-19")
}
