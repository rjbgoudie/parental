\name{bn.list}
\alias{bn.list}
\title{Constructor function for a 'bn list'.}
\usage{bn.list(...)}
\description{Constructor function for a 'bn list'.}
\details{Constructs bn.list objects}
\value{An object of class 'bn.list'}
\seealso{\code{\link{[.bn.list}}, \code{\link{c.bn.list}}}
\arguments{\item{...}{A series of objects of class 'bn'.}}
\examples{p1 <- bn(c(), c(1), c(1, 2, 4), c())
p2 <- bn(c(2), c(), c(1), c())
x <- bn.list(p1, p2)
x[1]
x[[1]]
x[2]}
