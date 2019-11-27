\name{nxday}
\alias{nyday}
\alias{nsday}
\alias{nqday}
\alias{nmday}
\alias{nwday}
\title{
  Obtain Day Count Information 
}
\description{
  Obtain the number of days of the period specified by an FDate object.
  Return the number of days in the year for \code{nyday}, the number of
  days in the semester for \code{nsday}, the number of days in the
  quarter for \code{nqday}, the number of days in the month for
  \code{nmday} and the number of days in the week for \code{nwday}.
}
\usage{
nyday(x, \dots)
nsday(x, \dots)
nqday(x, \dots)
nmday(x, \dots)
nwday(x, \dots)
}
\arguments{
  \item{x}{vector of FDate objects.}
  \item{\dots}{further arguments, currently ignored}
}
\value{
  Integer value(s) representing the number of days in the period.
}
\seealso{\code{\link{yday}} \code{\link{sday}} \code{\link{qday}} \code{\link{mday}} \code{\link{wday}}}
\examples{
  nyday(as.FDate("2014-05-19"))
  nqday(as.FDate("2014-05-19"))
  nmday(as.FDate("2014-05-19"))
}
