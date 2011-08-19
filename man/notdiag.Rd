\name{notdiag}
\alias{notdiag}
\title{Retrieve off-diagonal elements.}
\usage{
  notdiag(mx)
}
\arguments{
  \item{mx}{A matrix}
}
\value{
  A vector of off-diagonals
}
\description{
  Retrieve off-diagonal elements
}
\examples{
m <- matrix(1:9, 3, 3)
notdiag(m)
}
\seealso{
  \code{\link{diag}}, \code{\link{lower.tri}},
  \code{\link{upper.tri}}
}

