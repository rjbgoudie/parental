\name{as.bn.graphNEL}
\alias{as.bn.graphNEL}
\title{Convert a graphNEL object to a 'bn' object.}
\usage{\method{as.bn}{graphNEL} (x, ...)}

\description{
  Convert a graphNEL object to a 'bn' object.
}

\details{
  graphNEL objects are from bioconductor package 'graph'

  Note that graphNEL's edge list are *children* lists.
}
\value{An object of class 'bn'.}
\arguments{
  \item{x}{An object of class 'graphNEL'}
  \item{...}{unused}
}
\examples{if (require(graph)){
set.seed(123)
c1 <- c(1, 1, 2, 4)
names(c1) <- as.character(1:4)
g1 <- randomNodeGraph(c1)
as.parental(g1)
}}
