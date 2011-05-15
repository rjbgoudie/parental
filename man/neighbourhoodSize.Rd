\name{neighbourhoodSize}
\alias{neighbourhoodSize}
\title{Neighbourhood size...}
\usage{neighbourhoodSize(x)}
\description{Neighbourhood size}
\details{Computes the number of DAGs that can be reach by adding, removing or 
reversing the direction (?) of a single arc}
\value{The size of the neighbourhood (an integer)}
\arguments{\item{x}{An object of class \code{parental}}}
\examples{x <- bn(c(), c(3), c(1), c(1, 2))
neighbourhoodSize(x)}
