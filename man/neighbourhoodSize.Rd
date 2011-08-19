\name{neighbourhoodSize}
\alias{neighbourhoodSize}
\title{Neighbourhood size.}
\usage{
  neighbourhoodSize(x)
}
\arguments{
  \item{x}{An object of class \code{parental}}
}
\value{
  The size of the neighbourhood (an integer)
}
\description{
  Computes the number of DAGs that can be reach by adding,
  removing or reversing the direction (?) of a single arc
}
\examples{
x <- bn(c(), c(3), c(1), c(1, 2))
neighbourhoodSize(x)
}

