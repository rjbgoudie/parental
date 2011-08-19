\name{is.valid.bn}
\alias{is.valid.bn}
\title{Check validity.}
\usage{
  \method{is.valid}{bn} (x)
}
\arguments{
  \item{x}{A object of class 'bn'}
}
\value{
  A logical of length 1 indicating whether x is a valid
  'bn' object
}
\description{
  Checks whether the supplied 'bn is valid. Tests that the
  parents are sorted correctly, are of storage.mode() ==
  "integer". Additionally checks that the supplied bn is
  acyclic.
}
\examples{
x <- bn(c(), 1, 2)
is.valid(x)
}
\seealso{
  \code{\link{is.valid.parental}}
}

