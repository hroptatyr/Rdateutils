\name{dateutils-package}
\alias{dateutils}
\alias{dateutils-package}
\title{Handling dates in R}
\description{
  Routines to extend R's date handling by proper
  accessors (year, month, mday, etc.) and arithmetic (date
  plus 1 year).  Spurious dates arising in financial contexts
  like "2019-A", "2019-10" or "2019-Q4" are handled alongside
  `ordinary' dates.
  Routines are implemented in C and mostly parallelised using
  OpenMP for high throughput scenarios.
}
\author{
Sebastian Freundt  <freundt@ga-group.nl>
}
