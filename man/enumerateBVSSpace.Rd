\name{enumerateBVSSpace}
\alias{enumerateBVSSpace}
\title{Enumerate the space of BVSs.}
\usage{enumerateBVSSpace(numberOfNodes, response,
    maxNumberParents=numberOfNodes - 1)}

\description{
  Enumerate the space of BVSs.
}

\details{
  Enumerate (ie make a list of, not just count) the entire
  space of Bayesian Variable Selection models, on a given
  number of nodes, for a particular response. Optionally,
  restrcit the maximum number of parents
}
\value{An object of class "bvs.list", a list of objects of class "bvs"}
\arguments{
  \item{numberOfNodes}{An integer of length 1. The number of noes}
  \item{response}{An integer of length 1. Which node is the response}
  \item{maxNumberParents}{The maximum number of parents of node response.}
}
