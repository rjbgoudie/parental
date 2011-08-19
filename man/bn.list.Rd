\name{bn.list}
\alias{bn.list}
\title{Constructor function for a 'bn list'.}
\usage{
  bn.list(...)
}
\arguments{
  \item{...}{A series of objects of class 'bn'.}
}
\value{
  An object of class 'bn.list'
}
\description{
  Constructs bn.list objects
}
\examples{
p1 <- bn(c(), c(1), c(1, 2, 4), c())
p2 <- bn(c(2), c(), c(1), c())
x <- bn.list(p1, p2)
x[1]
x[[1]]
x[2]
}
\seealso{
  \code{\link{[.bn.list}}, \code{\link{c.bn.list}}
}

