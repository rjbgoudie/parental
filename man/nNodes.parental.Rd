\name{nNodes.parental}
\alias{nNodes.parental}
\title{Number of nodes/vertices.}
\usage{
  \method{nNodes}{parental} (x, ...)
}
\arguments{
  \item{x}{An object of class \code{parental}}

  \item{...}{Further arguments (unused)}
}
\value{
  The number of nodes in \code{parental}, an integer.
}
\description{
  Get the number of nodes in a \code{parental} object
}
\examples{
x <- parental(c(), c(3), c(1), c(1, 2))
nNodes(x)
}

