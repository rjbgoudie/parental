\name{ptablesDirichlet}
\alias{ptablesDirichlet}
\title{Draw ptables from product Dirichlet distribution.}
\usage{ptablesDirichlet(alpha, nlevels)}

\description{
  Draw ptables from product Dirichlet distribution.
}

\details{
  Generate a conditional probability table from a product
  Dirichlet distribution.
}
\value{A table of the form required for \code{\link{simulate.bn}}.}
\arguments{
  \item{alpha}{Vector containing shape parameters.}
  \item{nlevels}{The number of levels of each of the parents of the node.}
}
\examples{ptablesDirichlet(c(1, 2), c(3, 4))}
