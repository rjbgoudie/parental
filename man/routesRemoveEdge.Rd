\name{routesRemoveEdge}
\alias{routesRemoveEdge}
\title{Update a routes matrix (edge removal).}
\usage{
  routesRemoveEdge(x, i, j)
}
\arguments{
  \item{x}{A routes matrix}

  \item{i}{The node from which the removed edge emanates}

  \item{j}{The node that the removed edge goes to}
}
\description{
  A routes matrix is a matrix A, such that each element (i,
  j) is the number of routes from i to j in some directed
  graph.
}
\details{
  This function updates the routes matrix to account for
  the deletion of an edge from i to j in the directed graph
}
\examples{
x1 <- bn(c(), c(3), c(1), c(1, 2))
x2 <- bn(c(), c(3), c(1), c(1, 2, 3))

y <- routes(x2)
routesRemoveEdge(y, 3, 4)
routes(x1)
}
\seealso{
  \code{\link{routes}}, \code{\link{routesAddEdge}}
}

