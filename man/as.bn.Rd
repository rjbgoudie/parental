\name{as.bn}
\alias{as.bn}
\title{Convert an object to a bn.}
\usage{
  as.bn(x, ...)
}
\arguments{
  \item{x}{An object}

  \item{...}{Further arguments, passed to method}
}
\description{
  A generic function that converts objects to \code{bn}
  objects
}
\examples{
edgelist <- matrix(c(c(1,2), c(2, 3)), byrow = TRUE, nrow = 2, ncol = 2)
colnames(edgelist) <- c("row", "col")
as.bn(edgelist, type = "edgelist", n = 3)

x <- "[][1][1,2][3]"
as.bn(x, pretty = TRUE)

if (require(graph)){
set.seed(123)
c1 <- c(1, 1, 2, 4)
names(c1) <- as.character(1:4)
g1 <- randomNodeGraph(c1)
as.parental(g1)
}
}
\seealso{
  \code{\link{as.bn.matrix}},
  \code{\link{as.bn.character}},
  \code{\link{as.bn.graphNEL}}
}

