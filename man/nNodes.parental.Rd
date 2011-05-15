\name{nNodes.parental}
\alias{nNodes.parental}
\title{Number of nodes/vertices.}
\usage{\method{nNodes}{parental} (x, ...)}
\description{Number of nodes/vertices.}
\details{Get the number of nodes in a \code{parental} object}
\value{The number of nodes in \code{parental}, an integer.}
\arguments{\item{x}{An object of class \code{parental}}
\item{...}{Further arguments (unused)}}
\examples{x <- parental(c(), c(3), c(1), c(1, 2))
nNodes(x)}
