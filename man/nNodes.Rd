\name{nNodes}
\alias{nNodes}
\title{Number of nodes.}
\usage{
  nNodes(x, ...)
}
\arguments{
  \item{x}{A graph object}

  \item{...}{Further arguments, passed to method}
}
\value{
  The number of nodes in \code{parental}, an integer.
}
\description{
  A generic to get the number of nodes of a graph object.
}
\examples{
x <- parental(c(), c(3), c(1), c(1, 2))
nNodes(x)
}
\seealso{
  \code{\link{nNodes.parental}}
}

