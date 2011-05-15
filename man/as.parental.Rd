\name{as.parental}
\alias{as.parental}
\title{Convert an object to a parental object...}
\usage{as.parental(x, ...)}
\description{Convert an object to a parental object}
\details{A generic function for converting objects to \code{\link{parental}} 
objects.}
\seealso{\code{\link{as.parental.bn}}, \code{\link{as.parental.matrix}},
\code{\link{as.parental.character}}, \code{\link{as.parental.graphNEL}}.}
\arguments{\item{x}{An object}
\item{...}{Further arguments passed to method}}
\examples{x <- bn(c(), c(1), c(1, 2))
as.parental(x)

adjmat <- matrix(sample(c(0, 1), 100, rep = TRUE), 10, 10)
diag(adjmat) <- 0
as.parental(adjmat)

edgelist <- matrix(c(1, 2), nrow = 1, ncol = 2)
colnames(edgelist) <- c("row", "col")
as.parental(edgelist, type = "edgelist", n = 2)

x <- "[3][1][2][1][1,3]"
as.parental(x, pretty = TRUE)

x <- "3,1,2,1,c(1,3)"
as.parental(x)

if (require(graph)){
set.seed(123)
c1 <- c(1, 1, 2, 4)
names(c1) <- as.character(1:4)
g1 <- randomNodeGraph(c1)
as.parental(g1)
}}
