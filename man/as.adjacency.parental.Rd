\name{as.adjacency.parental}
\alias{as.adjacency.parental}
\title{Convert a object of class 'parental' to an adjacency matrix.}
\usage{
  \method{as.adjacency}{parental} (x, ...)
}
\arguments{
  \item{x}{A object of class 'parental'.}

  \item{...}{unused}
}
\value{
  An adjacency matrix.
}
\description{
  An adjacency matrix is a matrix A of dimension n x n,
  with A[i, j] = 1 if an edge exist between nodes i and j,
  where n is the number of nodes.
}
\examples{
x <- parental(c(), c(1), c(2))
as.adjacency(x)
}

