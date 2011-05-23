\name{as.graph.parental}
\alias{as.graph.parental}
\title{Convert a 'parental' object to a 'graphNEL' object.}
\usage{\method{as.graph}{parental} (x, ...)}

\description{
  Convert a 'parental' object to a 'graphNEL' object.
}

\details{
  graphNEL objects are from bioconductor package 'graph'

  Note that graphNEL's edge list are *children* lists.
}
\value{An object of class 'graphNEL'.}
\arguments{
  \item{x}{An object of class 'parental'}
  \item{...}{unused}
}
\examples{if (require(graph)){
x <- bn(integer(0), 1, 2)
y <- as.graph(x)
edges(y)
}}
