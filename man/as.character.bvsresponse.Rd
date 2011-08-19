\name{as.character.bvsresponse}
\alias{as.character.bvsresponse}
\title{Convert 'bvsresponse' to character vector.}
\usage{
  \method{as.character}{bvsresponse} (x, ...)
}
\arguments{
  \item{x}{An object of class 'bvsresponse'}

  \item{...}{Further arguments (unused)}
}
\value{
  An object of class 'parental'.
}
\description{
  Convert a 'bvsresponse' object to a character string.
  Note that the number of nodes, and which node the
  response is not contained in the resulting character
  string.
}
\details{
  For example, as.character() on a bvsresponse with 10
  nodes, response node 5, which has parents 2, 3, 6, and 10
  returns "2,3,6,10".
}

