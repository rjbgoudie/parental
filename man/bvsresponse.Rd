\name{bvsresponse}
\alias{bvsresponse}
\title{Constructor function for a 'bvsresponse' object.}
\usage{bvsresponse(x, response, nNodes)
}
\description{Constructor function for a 'bvsresponse' object.
Objects of class 'bvsresponse' are lists, with the following components:}
\details{$parents: A vectors of integers, specifying the parents of the response. 
The parents must be specified as a vector of integers -- the 
storage.mode() of these integers MUST be integer. If the node has no 
parents, use integer(0).

Note, in particular, that c(2, 1) would not be a valid specification 
of 'parents', because storage.mode(2) and storage.mode(1) == "double"
Instead one would use c(2L, 1L), as documented in the R FAQ (see [1])

Parents of each node MUST be sorted in increasing order.
Conformance with object requirements can be tested by is.valid()

[1] http://cran.r-project.org/doc/manuals/R-lang.html#Constants}
\value{An object of class 'bvsresponse'.}
\arguments{\item{x}{A vector of integers specifying the parents of the response.}
\item{response}{A integer of length 1 specifying which node (column) is the response.}
\item{nNodes}{The number of nodes (columns) in the variable selection.}
}

