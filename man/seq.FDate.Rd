\name{seq.FDate}
\alias{seq.FDate}
\title{
  Sequence of FDates
}
\description{
  Generate a sequence of FDates.
}
\usage{
 \method{seq}{FDate}(from, till, by, from.last, \dots)
}
\arguments{
  \item{from}{First element of sequence}
  \item{till}{Last element of sequence}
  \item{by}{Increment}
  \item{from.last}{Logical, whether to end up on the last element}
  \item{\dots}{additional arguments, ignored}
}
\value{
  Vector of FDates.
}
\examples{
  seq.FDate("2000-01-01","2001-12-31")
  seq.FDate("2000-01-31","2000-12-31", by="1M")
}
