\name{pdur}
\alias{pdur}
\title{
  Durations According to Period Lengths
}
\description{
  Calculate duration of a period FDate.
}
\usage{
  pdur(x, type=c("ddur", "character", "raw"))
}
\arguments{
  \item{x}{vector of FDates}
  \item{type}{output type}
}
\value{
  Vector of ddurs (for type ddur), vector of character (for type character),
  vector of integer (for type raw).\cr
  Represents the duration of the period assumed by x.
}
\examples{
  pdur(as.FDate("2019-09-30"))
  pdur(as.FDate("2018-10"))
  pdur(as.FDate("2018-S2"))
}
