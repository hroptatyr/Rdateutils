\name{accessor}
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
\alias{dday}
\alias{year<-}
\alias{yday<-}
\alias{qday<-}
\alias{month<-}
\alias{mday<-}
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
\alias{qday<-.FDate}
\alias{month<-.FDate}
\alias{mday<-.FDate}
\alias{year.ddur}
\alias{semi.ddur}
\alias{quarter.ddur}
\alias{month.ddur}
\alias{week.ddur}
\alias{dday.ddur}
\title{
  Access date or duration components
}
\description{
  Access components of a date or duration.
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
dday(x, \dots)
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
\method{year}{ddur}(x, \dots)
\method{semi}{ddur}(x, \dots)
\method{quarter}{ddur}(x, \dots)
\method{month}{ddur}(x, \dots)
\method{week}{ddur}(x, \dots)
\method{dday}{ddur}(x, \dots)
}
\arguments{
  \item{x}{vector of date or duration objects.}
  \item{\dots}{further arguments, currently ignored}
}
\value{
  Integer value(s) of the date or duration component.
}
\keyword{classes}
\keyword{manip}
\seealso{\code{\link{months}} \code{\link{weekdays}}}
\examples{
  year.FDate("2014-05-19")
}
