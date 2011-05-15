\name{as.parental.matrix}
\alias{as.parental.matrix}
\title{Convert a variety of matrix objects to 'parental' objects.}
\usage{\method{as.parental}{matrix} (x, type="adjacency", n, ...)}
\description{Convert a variety of matrix objects to 'parental' objects.}
\details{The supplied matrix x should either be an adjacency 
matrix (type = "adjacency"), or an edgelist (type = "edgelist").
Adjacency matrices should be an n x n matrix A with A[i, j] = 1 if an 
edge exists between node i and j. Edgelists are of the form returned 
by which(x == condition, arr.ind = TRUE) for matrices x in 
adjacency matrix form.

For type = "edgelist", the number of nodes n must be specified.}
\value{An object of class 'parental'.}
\arguments{\item{x}{An object of class 'matrix'}
\item{type}{Either "adjacency" or "edgelist"}
\item{n}{The number of nodes n. Only required for type = "edgelist"}
\item{...}{Further arguments (unused)}}
\examples{adjmat <- matrix(sample(c(0, 1), 100, rep = T), 10, 10)
diag(adjmat) <- 0
as.parental(adjmat)

edgelist <- matrix(c(1, 2), nrow = 1, ncol = 2)
colnames(edgelist) <- c("row", "col")
as.parental(edgelist, type = "edgelist", n = 2)}
