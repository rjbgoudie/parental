\name{numberOfMovesBetweenIgnoringCycles}
\alias{numberOfMovesBetweenIgnoringCycles}
\title{Number of moves between graphs.}
\usage{
  numberOfMovesBetweenIgnoringCycles(x, y, components = F,
  allowFlips = F)
}
\arguments{
  \item{x}{An object of class "parental"}

  \item{y}{An object of class "parental"}

  \item{components}{A logical of length 1, indicating
  whether the total number of moves required to morph x to
  y should be returned (components = F) or if the number of
  changes required for each node should be returned
  (components = T).}

  \item{allowFlips}{Allow single-edge flip moves. This is
  not compatible with components = T, because it is not
  clear for which node to account flip moves.}
}
\value{
  if components == FALSE: A numeric of length 1 indicating
  the number of moves required. if components == TRUE: A
  number number of length (nNodes(x) == nNodes(y)). The
  figure in position i of the vector relates to the number
  changes that need to be made to change the inward bound
  edges toward node i.
}
\description{
  Compute the number of edge additions, removals (and
  single-edge flips if allowFlips = T) that would be
  required to morph from network x to network y, both of
  which are objects of class 'parental'. The measure is
  symmetric.
}
\details{
  The parental objects x and y must have the same number of
  nodes. No attempt is made to account for whether
  intermediate graphs are cyclic (but perhaps there is
  always one ordering of the moves that is OK?).
}
\examples{
bn1 <- bn(2, integer(0))
bn2 <- bn(integer(0), 1)
numberOfMovesBetweenIgnoringCycles(bn1, bn2)
}

