\name{indegrees.parental}
\alias{indegrees.parental}
\title{Indegrees.}
\usage{
  \method{indegrees}{parental} (x, ...)
}
\arguments{
  \item{x}{An object of class \code{parental}}

  \item{...}{Further arguments, currently unused}
}
\value{
  A vector of length \code{nNodes(x)}, with the indegrees
  of each node of \code{x}
}
\description{
  Get the indegree of each node of the supplied graph
}
\examples{
x <- parental(c(), c(3), c(1), c(1, 2))
indegrees(x)
}

