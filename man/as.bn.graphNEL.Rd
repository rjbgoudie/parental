\name{as.bn.graphNEL}
\alias{as.bn.graphNEL}
\title{Convert a graphNEL object to a 'bn' object.}
\usage{
  \method{as.bn}{graphNEL} (x, ...)
}
\arguments{
  \item{x}{An object of class 'graphNEL'}

  \item{...}{unused}
}
\value{
  An object of class 'bn'.
}
\description{
  graphNEL objects are from bioconductor package 'graph'
}
\details{
  Note that graphNEL's edge list are *children* lists.
}
\examples{
if (require(graph)){
set.seed(123)
c1 <- c(1, 1, 2, 4)
names(c1) <- as.character(1:4)
g1 <- randomNodeGraph(c1)
as.parental(g1)
}
}

