\name{as.graph}
\alias{as.graph}
\title{Convert an object to a graph.}
\usage{
  as.graph(x, ...)
}
\arguments{
  \item{x}{An object}

  \item{...}{Further arguments, passed to method}
}
\description{
  graphNEL objects are from bioconductor package 'graph'.
  See \link[graph]{graphNEL-class}.
}
\examples{
if (require(graph)){
x <- bn(integer(0), 1, 2)
y <- as.graph(x)
edges(y)
}
}
\seealso{
  \code{\link{as.graph.parental}}
}

