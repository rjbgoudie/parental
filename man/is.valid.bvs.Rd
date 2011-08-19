\name{is.valid.bvs}
\alias{is.valid.bvs}
\title{Check validity.}
\usage{
  \method{is.valid}{bvs} (x)
}
\arguments{
  \item{x}{A object of class 'bvs'}
}
\value{
  A logical of length 1 indicating whether x is a valid
  'bvs' object.
}
\description{
  Checks whether the supplied 'bvs' is valid. Tests that
  the parents are sorted correctly, are of storage.mode()
  == "integer". Additionally tests that there is a unique
  response.
}

