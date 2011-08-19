\name{nEdges}
\alias{nEdges}
\title{Number of edges.}
\usage{
  nEdges(parental)
}
\arguments{
  \item{parental}{An object of class \code{parental}}
}
\value{
  The number of edges in \code{parental}, an integer.
}
\description{
  Get the number of edges in a \code{parental} object
}
\examples{
x <- parental(c(), c(3), c(1), c(1, 2))
nEdges(x)
}

