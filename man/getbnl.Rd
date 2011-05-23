\name{getbnl}
\title{Extract parts of a 'bn list'.}
\usage{\method{[}{bn.list}(x, i)}

\description{
  Extract parts of a 'bn list'.
}

\details{
  Operator that acts on objects of class \code{bn.list} to
  extract parts of the list
}
\value{An object of class \code{bn.list}, consisting the parts of 
\code{x} indicated by \code{i}}
\alias{[.bn.list}
\seealso{\code{\link{bn.list}}}
\arguments{
  \item{x}{Object from which to extract element(s)}
  \item{i}{Indicies specifying elements to extract}
}
\examples{p1 <- bn(c(), c(1), c(1, 2, 4), c())
p2 <- bn(c(2), c(), c(1), c())
x <- bn.list(p1, p2)
x[1]
x[[1]]
x[2]}
