\name{as.FDate}
\alias{as.FDate}
\alias{as.FDate.character}
\alias{as.FDate.integer}
\alias{as.FDate.Date}
\alias{as.FDate.default}
\alias{as.IDate.FDate}
\alias{as.Date.FDate}
\alias{as.POSIXlt.FDate}
\title{
  Coerce to FDate
}
\description{
  Methods to coerce date inputs to FDate.

  For character vectors \code{as.FDate} parses ISO 8601 dates (of the form YYYY-MM-DD)
  as well as ISO 8601 inspired incomplete or spurious dates, see details.

  For integer vectors \code{as.FDate} assumes the form YearMonthDay with zero padding.

  All other inputs are passed to \code{\link{as.Date}} first.
}
\usage{
  as.FDate(x, \dots)
  \method{as.FDate}{character}(x, \dots)
  \method{as.FDate}{integer}(x, \dots)
  \method{as.FDate}{Date}(x, \dots)
  \method{as.FDate}{default}(x, \dots)
  \method{as.Date}{FDate}(x, \dots)
  \method{as.POSIXlt}{FDate}(x, \dots)
}
\arguments{
  \item{x}{vector of character, integer or Date}
  \item{\dots}{further arguments passed to \code{\link{as.Date}}}
}
\details{
  Spurious month dates in integer form can be expressed by using 00 for the day of month.
  In character form they look like "YYYY-MM".

  Spurious quarter dates are of the form "YYYY-Qn" where n ranges from 1 to 4.
  Semi dates are of the form "YYYY-Sn" where n is 1 or 2.
  Annual dates are of the form "YYYY-A" though for convenience the -A suffix may
  be omitted.
}
\value{
  Vector of FDates.
}
\keyword{classes}
\keyword{manip}
\seealso{\code{\link{as.Date}}}
\examples{
  as.FDate("2019-09-22")
  as.FDate(20190922L)
  as.FDate(Sys.Date())
  as.FDate(Sys.time())
}
