\name{parental2bnlearn}
\alias{parental2bnlearn}
\title{Conversion to 'bnlearn'.}
\usage{
  parental2bnlearn(x)
}
\arguments{
  \item{x}{A \code{parental} graph}
}
\value{
  x A 'bnlearn' object, of class \code{bn} (the bnlearn
  version).
}
\description{
  Convert a 'parental' object to a 'bnlearn' graph object
}
\details{
  This bridge to \code{bnlearn} can be temperamental
  because \code{bnlearn} also has an object called
  \code{bn}.
}

