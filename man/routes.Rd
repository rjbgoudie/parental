\name{routes}
\alias{routes}
\title{Get routes matrix for a 'bn'.}
\usage{routes(x)}

\description{
  Get routes matrix for a 'bn'.
}

\details{
  Returns a matrix encoding the number of routes between
  the nodes of the bn x.

  Element (i, j) contains the number of routes from node i
  to node j for i != j Element (i, i) contains 1 for all i.
}
\value{A matrix of dimension nNodes(x) x nNodes(x)}
\arguments{
  \item{x}{An object of class 'bn'.}
}
\examples{x <- bn(c(), c(3), c(1), c(1, 2))
routes(x)}
