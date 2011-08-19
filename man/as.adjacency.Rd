\name{as.adjacency}
\alias{as.adjacency}
\title{Convert an object to an adjacency matrix.}
\usage{
  as.adjacency(x, ...)
}
\arguments{
  \item{x}{An object}

  \item{...}{Further arguments, passed to method}
}
\description{
  A generic for converting objects to adjacency matrices.
}
\details{
  An adjacency matrix is a matrix A of dimension n x n,
  with A[i, j] = 1 if an edge exist between nodes i and j,
  where n is the number of nodes.
}
\examples{
x <- parental(c(), c(1), c(2))
as.adjacency(x)
}
\seealso{
  \code{\link{as.adjacency.parental}}
}

