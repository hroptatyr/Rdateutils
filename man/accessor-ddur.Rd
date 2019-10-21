\name{accessor-ddur}
\alias{dday}
\alias{year.ddur}
\alias{semi.ddur}
\alias{quarter.ddur}
\alias{month.ddur}
\alias{week.ddur}
\alias{dday.ddur}
\title{
  Access duration components
}
\description{
  Access components of a duration.
}
\usage{
dday(x, \dots)
\method{year}{ddur}(x, \dots)
\method{semi}{ddur}(x, \dots)
\method{quarter}{ddur}(x, \dots)
\method{month}{ddur}(x, \dots)
\method{week}{ddur}(x, \dots)
\method{dday}{ddur}(x, \dots)
}
\arguments{
  \item{x}{vector of duration objects.}
  \item{\dots}{further arguments, currently ignored}
}
\value{
  Integer value(s) of the duration component(s).
}
\keyword{classes}
\keyword{manip}
\examples{
  month.ddur("17M")
  dday.ddur("17M")
  dday.ddur("17D")
}
