\name{nNodes}
\alias{nNodes}
\title{Number of nodes...}
\usage{nNodes(x, ...)}
\description{Number of nodes}
\details{A generic to get the number of nodes of a graph object.}
\value{The number of nodes in \code{parental}, an integer.}
\seealso{\code{\link{nNodes.parental}}}
\arguments{\item{x}{A graph object}
\item{...}{Further arguments, passed to method}}
\examples{x <- parental(c(), c(3), c(1), c(1, 2))
nNodes(x)}
