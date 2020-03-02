\name{trunc.ddur}
\alias{trunc.ddur}
\title{
  Truncate ddurs
}
\description{
  Truncate ddurs such that they are multiplies of the specified unit.
}
\usage{
 \method{trunc}{ddur}(x, units=c("days","weeks","months","quarters","semis","years"), \dots)
}
\arguments{
  \item{x}{Vector of ddurs}
  \item{units}{Unit to truncate to, one of the values listed}
  \item{\dots}{additional arguments, ignored}
}
\value{
  Vector of ddurs.
}
\examples{
  trunc(as.ddur("P19M15D"), "year")
  trunc(as.ddur("P19M15D"), "semi")
  trunc(as.ddur("P19M15D"), "quarter")
  trunc(as.ddur("P19M15D"), "month")
}
