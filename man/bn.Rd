\name{bn}
\alias{bn}
\title{Constructor function for a 'bn' object.}
\usage{
  bn(...)
}
\arguments{
  \item{...}{A series of vectors specifying the parents of
  each node. These vectors must be of storage.mode
  "integer".}
}
\value{
  An object of class 'bn'.
}
\description{
  \code{bn} is a subclass of \code{parental}. See
  \code{\link{parental}} for detailed documentation.
}
\examples{
bn(c(), 1, 2)
}
\seealso{
  \code{\link{parental}}
}

