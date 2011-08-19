\name{parents}
\alias{parents}
\title{Parents.}
\usage{
  parents(x, i)
}
\arguments{
  \item{x}{The object for which to get the parents}

  \item{i}{Which node}
}
\value{
  The parents
}
\description{
  Parents.
}
\examples{
x <- parental(c(), 1, c(2, 4), c())
parents(x, 3)
}

