\name{pretty.FDate}
\alias{pretty.FDate}
\title{
  Pretty Breakpoints
}
\description{
  Compute a sequence of equally spaced round values which
  cover the input range.
}
\usage{
  \method{pretty}{FDate}(x, n=5L, min.n, \dots)
}
\arguments{
  \item{x}{Vector of FDates}
  \item{n}{The desired number of breakpoints.}
  \item{min.n}{The minimum number of breakpoints.}
  \item{\dots}{additional arguments, ignored}
}
\value{
  Vector of FDates.
}
\examples{
  trunc.FDate("2000-11-04","year")
  trunc.FDate("2000-11-04","month")
  trunc.FDate("2000-11-04","quarter")
}
