\name{checkAcyclic}
\alias{checkAcyclic}
\title{Acyclicity testing.}
\usage{
  checkAcyclic(parental)
}
\arguments{
  \item{parental}{An object of class \code{parental}}
}
\value{
  A logical. TRUE if \code{parental} is acyclic. FALSE if
  \code{parental} contains a cycle.
}
\description{
  Check for cycles in a directed graph.
}
\examples{
x <- parental(c(), c(3), c(1), c(1, 2))
checkAcyclic(x)
}

