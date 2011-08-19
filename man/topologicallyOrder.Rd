\name{topologicallyOrder}
\alias{topologicallyOrder}
\title{Topological ordering.}
\usage{
  topologicallyOrder(parental)
}
\arguments{
  \item{parental}{An object of class \code{parental}}
}
\value{
  A numeric vector. The order.
}
\description{
  Finds a permutation of the nodes 1, ..., p such that each
  all ancestors of each node have a lower place in the
  order.
}
\examples{
x <- parental(c(), c(3), c(1), c(1, 2))
topologicallyOrder(x)
}

