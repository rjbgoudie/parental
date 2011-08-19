\name{getChildren}
\alias{getChildren}
\title{Children lists.}
\usage{
  getChildren(parental)
}
\arguments{
  \item{parental}{An object of class \code{parental}}
}
\value{
  A list, the ith component of which is a numeric vector
  listing the nodes that are children of node i.
}
\description{
  Create a list, with each component listing the direct
  children of the corresponding node in the supplied
  parental.
}
\examples{
x <- parental(c(), c(3), c(1), c(1, 2))
getChildren(x)
}

