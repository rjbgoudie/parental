\name{indegrees}
\alias{indegrees}
\title{Indegrees.}
\usage{indegrees(x, ...)}

\description{
  Indegrees.
}

\details{
  A generic to get the indegree of each node of the
  supplied graph
}
\value{A vector of length \code{nNodes(x)}, with the indegrees of each 
node of \code{x}}
\seealso{\code{\link{indegrees.parental}}}
\arguments{
  \item{x}{An object of class \code{parental}}
  \item{...}{Further arguments, passed to method}
}
\examples{x <- parental(c(), c(3), c(1), c(1, 2))
indegrees(x)}
