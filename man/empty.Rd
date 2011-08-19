\name{empty}
\alias{empty}
\title{Return empty graph.}
\usage{
  empty(n, class = "parental", response)
}
\arguments{
  \item{n}{A integer of length 1, specifying the number of
  nodes}

  \item{class}{The class of the returned graph. Can be any
  of "parental", "bn", "bvs", or "bvsresponse".}

  \item{response}{For "bvsresponse", which node is the
  response}
}
\value{
  An empty graph of the specified class.
}
\description{
  Returns an empty graph with n nodes, of the given class
}
\examples{
empty(5)
empty(5, "bn")
}
\seealso{
  \code{\link{complete}}, \code{\link{parental}},
  \code{\link{bn}}
}

