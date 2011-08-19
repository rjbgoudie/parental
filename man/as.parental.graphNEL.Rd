\name{as.parental.graphNEL}
\alias{as.parental.graphNEL}
\title{Convert a graphNEL object to a 'parental' object.}
\usage{
  \method{as.parental}{graphNEL} (x, ...)
}
\arguments{
  \item{x}{An object of class 'graphNEL'}

  \item{...}{Further arguments (unused)}
}
\value{
  An object of class 'parental'.
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

