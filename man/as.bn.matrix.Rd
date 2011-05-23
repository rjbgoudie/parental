\name{as.bn.matrix}
\alias{as.bn.matrix}
\title{Convert a matrix to a 'bn'.}
\usage{\method{as.bn}{matrix} (x, ...)}

\description{
  Convert a matrix to a 'bn'.
}

\details{
  See for 'as.parental.matrix' for details
}
\value{An object of class 'bn'}
\arguments{
  \item{x}{A matrix, as described in as.parental.matrix()}
  \item{...}{Passed to as.parental.matrix}
}
\examples{edgelist <- matrix(c(c(1,2), c(2, 3)), byrow = TRUE, nrow = 2, ncol = 2)
colnames(edgelist) <- c("row", "col")
as.bn(edgelist, type = "edgelist", n = 3)}
