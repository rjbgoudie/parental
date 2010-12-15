\name{parental}
\alias{parental}
\title{Constructor function for a parental object.}
\usage{parental(...)}
\description{Constructor function for a \code{parental} object.}
\details{Objects of class \code{parental} are lists with the ith component specifying
the parents of node i. The parents must be specified as a vector of 
integers -- the storage.mode() of these integers MUST be integer. 
If the node has no parents, use integer(0).

Note, in particular, that \code{list(2, 1)} would not be a valid 
\code{parental}  object, because storage.mode(2) and 
\code{storage.mode(1) == "double"}. Instead one would use 
\code{list(2L, 1L)}, as documented in the R FAQ (see [1])

Parents of each node MUST be sorted in increasing order.
Conformance with object requirements can be tested by \code{is.valid()}

[1] http://cran.r-project.org/doc/manuals/R-lang.html#Constants}
\value{An object of class \code{parental}.}
\keyword{constructor}
\arguments{\item{...}{A series of vectors specifying the parents of each node. These 
vectors must be of \code{storage.mode} "integer".}}
\examples{parental(c(), 1, 2)

parental(c(), c(1, 3), c())}
