\name{as.wcnt}
\alias{as.wcnt}
\alias{as.wcnt.character}
\title{
  Coerce to wcnt.
}
\description{
  Methods to coerce inputs to wcnt.

  For character vectors \code{as.wcnt} parses weekday-count strings of the form
  [-]nX  where N is a (signed) integer and X a day of the week.
}
\usage{
 as.wcnt(x, \dots)
 \method{as.wcnt}{character}(x, \dots)
}
\arguments{
  \item{x}{vector of characters}
  \item{\dots}{further arguments}
}
\value{
  Vector of wcnts.
}
\keyword{classes}
\examples{
  as.wcnt("10M")
  as.wcnt("-A")
  as.wcnt("Sat")
}
