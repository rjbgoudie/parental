\name{as.character.bvsresponse.list}
\alias{as.character.bvsresponse.list}
\title{Convert 'bvsresponse list' to character vector.}
\usage{
  \method{as.character}{bvsresponse.list} (x, ...)
}
\arguments{
  \item{x}{An object of class 'bvsresponse.list'}

  \item{...}{Further arguments (unused)}
}
\value{
  A character vector, each component of which is a
  character representation of the parents of each
  'bvsresponse' included in x.
}
\description{
  Convert a 'bvsresponse.list' object to a character
  string. Note that the number of nodes, and which node the
  response is not contained in the resulting character
  string.
}
\details{
  This function passes the list to
  as.character.bvsresponse()
}

