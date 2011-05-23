\name{nEdges}
\alias{nEdges}
\title{Number of edges.}
\usage{nEdges(parental)}

\description{
  Number of edges.
}

\details{
  Get the number of edges in a \code{parental} object
}
\value{The number of edges in \code{parental}, an integer.}
\arguments{
  \item{parental}{An object of class \code{parental}}
}
\examples{x <- parental(c(), c(3), c(1), c(1, 2))
nEdges(x)}
