\name{add.undirected.backend}
\alias{add.undirected.backend}
\title{(Internal) Add undirected edge to a 'bnlearn' object.}
\usage{add.undirected.backend(from, to, arcs, debug=F)}

\description{
  (Internal) Add undirected edge to a 'bnlearn' object.
}

\details{
  This bridge to \code{bnlearn} can be temperamental
  because \code{bnlearn} also has an object called
  \code{bn}.
}
\arguments{
  \item{from}{A character string, the label of a node.}
  \item{to}{A character string, the label of a node.}
  \item{arcs}{A matrix of two columns of character strings}
  \item{debug}{a boolean value. If TRUE a lot of debugging output is 
printed; otherwise the function is completely silent.}
}
