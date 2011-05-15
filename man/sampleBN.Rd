\name{sampleBN}
\alias{sampleBN}
\title{Draw a 'random' BN.}
\usage{sampleBN(n, maxNumberParents)}
\description{Draw a 'random' BN.}
\details{Generates a BN, by choosing an order, then sampling a BN that respects 
that order}
\value{A new \code{\link{bn}}.}
\seealso{An alternative \code{\link{sampleBN2}}}
\arguments{\item{n}{The number of nodes for the Bayesian network. An integer.}
\item{maxNumberParents}{The maximum indegree of the network}}
\examples{sampleBN(5)
sampleBN(10)
sampleBN(10, 2)}
