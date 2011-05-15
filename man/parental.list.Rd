\name{parental.list}
\alias{parental.list}
\title{Constructor function for a 'parental list' object.}
\usage{parental.list(...)}
\description{Constructor function for a 'parental list' object.}
\value{An object of class 'parental.list'}
\seealso{\code{\link{parental}}, \code{\link{bn.list}}, 
\code{\link{[.parental.list}}, \code{\link{c.parental.list}}}
\arguments{\item{...}{A series of objects of class \code{parental}.}}
\examples{p1 <- parental(c(), c(1), c(1, 2, 4), c(3))
p2 <- parental(c(2), c(), c(4), c(2, 3))
x <- parental.list(p1, p2)}
