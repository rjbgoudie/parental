\name{as.graph}
\alias{as.graph}
\title{Convert an object to a graph.}
\usage{as.graph(x, ...)}

\description{
  Convert an object to a graph.
}

\details{
  graphNEL objects are from bioconductor package 'graph'.
  See \link[graph]{graphNEL-class}.
}
\seealso{\code{\link{as.graph.parental}}}
\arguments{
  \item{x}{An object}
  \item{...}{Further arguments, passed to method}
}
\examples{if (require(graph)){
x <- bn(integer(0), 1, 2)
y <- as.graph(x)
edges(y)
}}
