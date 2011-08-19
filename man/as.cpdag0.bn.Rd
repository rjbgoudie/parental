\name{as.cpdag0.bn}
\alias{as.cpdag0.bn}
\title{Convert 'bn' to CPDAG.}
\usage{
  \method{as.cpdag0}{bn} (x, ...)
}
\arguments{
  \item{x}{The parental BN to be converted.}

  \item{...}{Further arguments (unused)}
}
\value{
  The CPDAG as a parental. (Which will in general not be a
  BN)
}
\description{
  Convert a parental BN to a Completed Partially Directed
  Acyclic Graph (CPDAG).
}

