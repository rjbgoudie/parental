\name{getpl}
\title{Extract parts of a parental...}
\usage{\method{[}{parental.list}(x, i)}
\description{Extract parts of a parental.list}
\details{Operator that acts on objects of class \code{parental.list} to extract 
parts of the list}
\value{An object of class \code{parental.list}, consisting the parts of 
\code{x} indicated by \code{i}}
\alias{[.parental.list}
\seealso{\code{\link{parental.list}}, \code{\link{c.parental.list}}}
\arguments{\item{x}{Object from which to extract element(s)}
\item{i}{Indicies specifying elements to extract}}
\examples{p1 <- parental(c(), c(1), c(1, 2, 4), c(3))
p2 <- parental(c(2), c(), c(4), c(2, 3))
x <- parental.list(p1, p2)
x[1]
x[[1]]
x[2]}
