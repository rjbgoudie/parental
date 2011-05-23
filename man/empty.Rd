\name{empty}
\alias{empty}
\title{Return empty graph.}
\usage{empty(n, class="parental", response)}

\description{
  Return empty graph.
}

\details{
  Returns an empty graph with n nodes, of the given class
}
\value{An empty graph of the specified class.}
\seealso{\code{\link{complete}}, \code{\link{parental}}, \code{\link{bn}}}
\arguments{
  \item{n}{A integer of length 1, specifying the number of nodes}
  \item{class}{The class of the returned graph. Can be any of "parental", 
"bn", "bvs", or "bvsresponse".}
  \item{response}{For "bvsresponse", which node is the response}
}
\examples{empty(5)
empty(5, "bn")}
