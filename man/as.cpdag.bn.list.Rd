\name{as.cpdag.bn.list}
\alias{as.cpdag.bn.list}
\title{Convert 'bn list' to CPDAGs.}
\usage{\method{as.cpdag}{bn.list} (x, verbose=T, ...)}

\description{
  Convert 'bn list' to CPDAGs.
}

\details{
  Convert a bn.list to list of parental.list of Completed
  Partially Directed Acyclic Graph (CPDAG).
}
\value{A parental.list containing a list of CPDAGs of class CPDAG.}
\arguments{
  \item{x}{An object of class bn.list}
  \item{verbose}{Should a progress bar be shown?}
  \item{...}{Further arguments (unused)}
}
