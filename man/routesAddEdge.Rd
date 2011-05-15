\name{routesAddEdge}
\alias{routesAddEdge}
\title{Update a routes matrix (edge addition).}
\usage{routesAddEdge(x, i, j)}
\description{Update a routes matrix (edge addition).}
\details{A routes matrix is a matrix A, such that each element (i, j) is the 
number of routes from i to j in some directed graph.

This function updates the routes matrix to account for the addition of 
an edge from i to j in the directed graph}
\seealso{\code{\link{routes}}, \code{\link{routesRemoveEdge}}}
\arguments{\item{x}{A routes matrix}
\item{i}{The node from which the added edge emanates}
\item{j}{The node that the added edge goes to}}
\examples{x1 <- bn(c(), c(3), c(1), c(1, 2))
x2 <- bn(c(), c(3), c(1), c(1, 2, 3))

y <- routes(x1)
routesAddEdge(y, 3, 4)
routes(x2)}
