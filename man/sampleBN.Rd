\name{sampleBN}
\alias{sampleBN}
\title{Draw a 'random' BN.}
\usage{
  sampleBN(n, maxNumberParents = NULL)
}
\arguments{
  \item{n}{The number of nodes for the Bayesian network. An
  integer.}

  \item{maxNumberParents}{The maximum indegree of the
  network}
}
\value{
  A new \code{\link{bn}}.
}
\description{
  Generates a BN, by choosing an order, then sampling a BN
  that respects that order
}
\examples{
sampleBN(5)
sampleBN(10)
sampleBN(10, 2)
}
\seealso{
  An alternative \code{\link{sampleBN2}}
}

