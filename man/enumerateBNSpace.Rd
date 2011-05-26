\name{enumerateBNSpace}
\alias{enumerateBNSpace}
\title{Enumerate the space of BNs.}
\usage{enumerateBNSpace(n, allowCyclic=F, banned=vector("list",
    n), required=vector("list", n), maxIndegree=n - 1,
    multicore=F, check=T)}

\description{
  Enumerate the space of BNs.
}

\details{
  Enumerate (ie make a list of, not just count) the entire
  space of Bayesian Networks models, on a given number of
  nodes.
}
\value{A \code{parental.list} including ALL the directed acyclic 
graphs with \code{n} nodes.}
\arguments{
  \item{n}{The number of nodes}
  \item{allowCyclic}{A logical indicating whether cyclic directed graphs
should be filtered out.}
  \item{banned}{A list of length \code{n}. Each component indicates which 
nodes cannot be parents of the corresponding node.}
  \item{required}{A list of length \code{n}. Each component indicates
which nodes must be includes as parents of the corresponding node.}
  \item{maxIndegree}{The maximum indegree of all nodes. A numeric of length
1.}
  \item{multicore}{A logical specifying whether to use 
\link[multicore]{mclapply}.}
  \item{check}{Should the input be checked for validity?}
}
\examples{enumerateBNSpace(3)
enumerateBNSpace(3, banned = list(c(), 1, c()))

x <- enumerateBNSpace(3, allowCyclic = TRUE)
filterCyclic(x)}
