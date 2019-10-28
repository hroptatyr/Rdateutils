\name{accessor-FDate}
\alias{year}
\alias{yday}
\alias{semi}
\alias{sday}
\alias{quarter}
\alias{qday}
\alias{month}
\alias{mday}
\alias{week}
\alias{wday}
\alias{year<-}
\alias{yday<-}
\alias{sday<-}
\alias{qday<-}
\alias{month<-}
\alias{mday<-}
\alias{week<-}
\alias{wday<-}
\alias{year.FDate}
\alias{yday.FDate}
\alias{semi.FDate}
\alias{sday.FDate}
\alias{quarter.FDate}
\alias{qday.FDate}
\alias{month.FDate}
\alias{mday.FDate}
\alias{week.FDate}
\alias{wday.FDate}
\alias{year<-.FDate}
\alias{yday<-.FDate}
\alias{sday<-.FDate}
\alias{qday<-.FDate}
\alias{month<-.FDate}
\alias{mday<-.FDate}
\alias{week<-.FDate}
\alias{wday<-.FDate}
\title{
  Access date components
}
\description{
  Access components of a date.
}
\usage{
year(x, \dots)
yday(x, \dots)
semi(x, \dots)
sday(x, \dots)
quarter(x, \dots)
qday(x, \dots)
month(x, \dots)
mday(x, \dots)
week(x, \dots)
wday(x, \dots)
\method{year}{FDate}(x, \dots)
\method{yday}{FDate}(x, \dots)
\method{semi}{FDate}(x, \dots)
\method{sday}{FDate}(x, \dots)
\method{quarter}{FDate}(x, \dots)
\method{qday}{FDate}(x, \dots)
\method{month}{FDate}(x, \dots)
\method{mday}{FDate}(x, \dots)
\method{week}{FDate}(x, \dots)
\method{wday}{FDate}(x, \dots)

year(x, \dots) <- value
yday(x, \dots) <- value
month(x, \dots) <- value
mday(x, \dots) <- value
\method{year}{FDate}(x, \dots) <- value
\method{yday}{FDate}(x, \dots) <- value
\method{month}{FDate}(x, \dots) <- value
\method{mday}{FDate}(x, \dots) <- value
}
\arguments{
  \item{x}{vector of date objects.}
  \item{\dots}{further arguments, currently ignored}
  \item{value}{integer value}
}
\value{
  Integer value(s) of the date component.
}
\keyword{classes}
\keyword{manip}
\seealso{\code{\link{months}} \code{\link{weekdays}}}
\examples{
  year.FDate("2014-05-19")
  yday.FDate("2014-01-19")
  semi.FDate("2014-07-19")
  sday.FDate("2014-07-19")
  quarter.FDate("2014-04-19")
  qday.FDate("2014-04-19")
  month.FDate("2014-05-19")
  mday.FDate("2014-05-19")
  week.FDate("2014-01-09")
  wday.FDate("2014-01-09")
}
