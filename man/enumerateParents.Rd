\name{enumerateParents}
\alias{enumerateParents}
\title{Enumerate all possible parents.}
\usage{
  enumerateParents(potentialParents, maxNumberParents =
  length(potentialParents), required = integer(0))
}
\arguments{
  \item{potentialParents}{A numeric vector of possible
  parents. Note: at the moment, this should EXCLUDE any
  required parents.}

  \item{maxNumberParents}{The maximum number of parents of
  node response.}

  \item{required}{A numeric vector of required parents}
}
\value{
  A list of possible parents sets, each of which will be a
  sorted numeric vector, stored as integers.
}
\description{
  Enumerate all the combinations of possible parents, given
  a SET of possible parents. Optionally, restrcit the
  maximum number of parents. Optionally, require that a
  particular parent is always present.
}

