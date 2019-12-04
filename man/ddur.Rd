\name{ddur}
\alias{ddur}
\alias{ydur}
\title{
  Durations According to Day Count Conventions
}
\description{
  Calculate duration in days or fractional years between dates.
}
\usage{
  ddur(x, y, day.count=c("act", "30", "30A", "30E", "30I", "30U", "bd", "28"))
  ydur(x, y, day.count=c("act/act", "30/360", "30A/360", "30E/360", "30I/360", "30U/360", "act/360", "act/365", "act/365A", "bd/252", "28/360"))
}
\arguments{
  \item{x}{vector of FDates}
  \item{y}{vector of FDates}
  \item{day.count}{day count convention}
}
\value{
  Vector of ddurs (for ddur), vector of numerics (for ydur).\cr
  Represents the duration (in days or years) between x and y according
  to the day count convention.
}
\examples{
  ddur(as.FDate("2019-09-30"), as.FDate("2019-10-03"))
  ddur(as.FDate("2018-10-31"), as.FDate("2019-04-30"), "30")
  ddur(as.FDate("2018-10-31"), as.FDate("2019-04-30"), "30E")

  ydur(as.FDate("2016-01-30"), as.FDate("2019-10-03"))
  ydur(as.FDate("2016-01-30"), as.FDate("2019-10-03"), "act/360")
  ydur(as.FDate("2016-01-30"), as.FDate("2019-10-03"), "30/360")
}
