\name{trunc.FDate}
\alias{trunc.FDate}
\title{
  Truncate FDates
}
\description{
  Generate truncated FDates where information coarser than specified is kept.
}
\usage{
 \method{trunc}{FDate}(x, units=c("days","weeks","months","quarters","semis","years"), \dots)
}
\arguments{
  \item{x}{Vector of FDates}
  \item{units}{Unit to truncate to, one of the values listed}
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
