\name{ptablesDirichlet}
\alias{ptablesDirichlet}
\title{Draw ptables from product Dirichlet distribution.}
\usage{
  ptablesDirichlet(alpha, nlevels)
}
\arguments{
  \item{alpha}{Vector containing shape parameters.}

  \item{nlevels}{The number of levels of each of the
  parents of the node.}
}
\value{
  A table of the form required for
  \code{\link{simulate.bn}}.
}
\description{
  Generate a conditional probability table from a product
  Dirichlet distribution.
}
\examples{
ptablesDirichlet(c(1, 2), c(3, 4))
}

