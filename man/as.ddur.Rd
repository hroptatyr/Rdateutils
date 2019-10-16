\name{as.ddur}
\alias{as.ddur}
\alias{as.ddur.character}
\alias{as.ddur.numeric}
\title{
  Coerce to ddur
}
\description{
  Methods to coerce duration inputs to ddur. 

  For character vectors \code{as.ddur} parses ISO 8601 duration strings.

  For numeric vectors \code{as.ddur} assumes a day duration.
}
\usage{
 as.ddur(x, \dots)
 \method{as.ddur}{character}(x, \dots)
 \method{as.ddur}{numeric}(x, \dots)
}
\arguments{
  \item{x}{vector of characters or numerics}
  \item{\dots}{further arguments}
}
\value{
  Vector of ddurs.
}
\keyword{classes}
\keyword{manip}
\examples{
  as.ddur("P6M")
  as.ddur(-22)
}
