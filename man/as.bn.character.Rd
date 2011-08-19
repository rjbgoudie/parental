\name{as.bn.character}
\alias{as.bn.character}
\title{Convert a character vector to a 'bn'.}
\usage{
  \method{as.bn}{character} (x, checkAcyclic = T, ...)
}
\arguments{
  \item{x}{A character object}

  \item{checkAcyclic}{Should the bn be checked for cycles?
  A logical.}

  \item{...}{Further arguments (unused)}
}
\value{
  A object of class \code{bn}.
}
\description{
  Format required is one of two. Either standard or pretty.
  See examples.
}
\examples{
x <- "[][1][1,2][3]"
as.bn(x, pretty = TRUE)
}

