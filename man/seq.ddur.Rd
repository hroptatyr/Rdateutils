\name{seq.ddur}
\alias{seq.ddur}
\title{
  Sequence of durations
}
\description{
  Generate a sequence of durations.
}
\usage{
 \method{seq}{ddur}(from, till, by, from.last, \dots)
}
\arguments{
  \item{from}{First element of sequence}
  \item{till}{Last element of sequence}
  \item{by}{Increment}
  \item{from.last}{Logical, whether to end up on the last element}
  \item{\dots}{additional arguments, ignored}
}
\value{
  Vector of ddurs.
}
\examples{
  seq.ddur("1M","10M")
  seq.ddur("0","4M4D",by="P1M1D")
}
