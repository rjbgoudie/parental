\name{bnlearn2parental}
\alias{bnlearn2parental}
\title{Conversion from 'bnlearn' object.}
\usage{bnlearn2parental(x)}

\description{
  Conversion from 'bnlearn' object.
}

\details{
  Convert a 'bnlearn' graph object to a 'parental' object.

  This bridge to \code{bnlearn} can be temperamental
  because \code{bnlearn} also has an object called
  \code{bn}.
}
\value{An object of class 'bn'.
bnlearn2parental()}
\arguments{
  \item{x}{A 'bnlearn' object, of class \code{bn} (the bnlearn version).}
}
