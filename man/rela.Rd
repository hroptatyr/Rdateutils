\name{rela}
\alias{\%after\%}
\alias{\%after|on\%}
\alias{\%before\%}
\alias{\%before|on\%}
\alias{older.than}
\alias{\%older.than\%}
\alias{newer.than}
\alias{\%newer.than\%}
\title{
  Compare dates or durations
}
\description{
  Relate two dates or durations to each other.
}
\usage{
  x \%after\% y
  x \%after|on\% y
  x \%before\% y
  x \%before|on\% y
  older.than(x, y, today, \dots)
  x \%older.than\% y
  newer.than(x, y, today, \dots)
  x \%newer.than\% y
}
\arguments{
  \item{x}{date or duration objects.}
  \item{y}{date or duration objects.}
  \item{today}{pivot date, defaults to Sys.Date()}
  \item{\dots}{additional arguments, currently ignored}
}
\value{
  Logical values.
}
\examples{
  as.FDate("2014-05-19") %after% as.FDate("2000-01-01")
  as.FDate("2014-05-19") %older.than% "3M"
}
