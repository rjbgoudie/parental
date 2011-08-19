\name{indegrees}
\alias{indegrees}
\title{Indegrees.}
\usage{
  indegrees(x, ...)
}
\arguments{
  \item{x}{An object of class \code{parental}}

  \item{...}{Further arguments, passed to method}
}
\value{
  A vector of length \code{nNodes(x)}, with the indegrees
  of each node of \code{x}
}
\description{
  A generic to get the indegree of each node of the
  supplied graph
}
\examples{
x <- parental(c(), c(3), c(1), c(1, 2))
indegrees(x)
}
\seealso{
  \code{\link{indegrees.parental}}
}

