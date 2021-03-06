\name{c.bn.list}
\alias{c.bn.list}
\title{Concatenates 'bn list'.}
\usage{
  \method{c}{bn.list} (...)
}
\arguments{
  \item{...}{Any number of 'bn.list' objects}
}
\value{
  An new 'bn.list' object, including all the supplied
  bn.lists
}
\description{
  Concatenates 'bn list'.
}
\examples{
p1 <- bn(c(), c(1), c(1, 2, 4), c())
p2 <- bn(c(2), c(), c(1), c())
x <- bn.list(p1, p2)
y <- bn.list(p2, p2, p1)
c(x, y)
}
\seealso{
  \code{\link{bn.list}}, \code{\link{[.bn.list}}
}

