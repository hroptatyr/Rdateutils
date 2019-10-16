\name{oldnew}
\alias{oldest}
\alias{newest}
\title{
  Oldest or newest elements
}
\description{
  Scan a vector of dates for the oldest or newest elements that fit in a range.
}
\usage{
 oldest(dates, span, which)
 newest(dates, span, which)
}
\arguments{
  \item{dates}{vector of FDates}
  \item{span}{a single duration}
  \item{which}{logical, default FALSE}
}
\value{
  Vector of logicals, or integers indicating the indices in case \code{which} is TRUE.
}
\examples{
  x <- seq.FDate("2000-01-01","2001-12-31")
  newest(x, "P3M")
  oldest(x, 17L, which=TRUE)
}
