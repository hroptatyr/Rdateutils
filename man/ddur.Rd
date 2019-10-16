\name{ddur}
\alias{ddur}
\title{
  Actual durations
}
\description{
  Calculate actual duration between dates.
}
\usage{
  ddur(x, y)
}
\arguments{
  \item{x}{vector of FDates}
  \item{y}{vector of FDates}
}
\value{
  Vector of ddurs.\cr
  Represents the actual duration (in days) between x and y.
}
\examples{
  ddur(as.FDate("2019-09-30"), as.FDate("2019-10-03"))
}
