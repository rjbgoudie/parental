\name{is.valid.parental}
\alias{is.valid.parental}
\title{Check validity.}
\usage{
  \method{is.valid}{parental} (x)
}
\arguments{
  \item{x}{A object of class \code{parental}}
}
\value{
  A logical of length 1 indicating whether x is a valid
  \code{parental} object
}
\description{
  Checks whether the supplied \code{parental} is valid.
  Tests that the parents are sorted correctly, are of
  storage.mode() == "integer"
}
\examples{
x <- parental(c(), c(1), c(1, 2, 4), c(3))
is.valid(x)
}
\seealso{
  \code{\link{is.valid.bn}}
}

