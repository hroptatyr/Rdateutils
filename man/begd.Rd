\name{begd}
\alias{begd}
\alias{endd}
\alias{begd.FDate}
\alias{endd.FDate}
\title{
  When viewed as truncated turn FDates back into instants.
}
\description{
  Untruncate FDates, align either to the beginning or the end of the period described by a truncated FDate.
}
\usage{
 begd(x, \dots)
 endd(x, \dots)
 \method{begd}{FDate}(x, \dots)
 \method{endd}{FDate}(x, \dots)
}
\arguments{
  \item{x}{Vector of FDates}
  \item{\dots}{additional arguments, ignored}
}
\value{
  Vector of FDates.
}
\examples{
  endd(as.FDate("2000-11"))
  begd(as.FDate("2000-11"))
  endd(as.FDate("2000-Q3"))
}
