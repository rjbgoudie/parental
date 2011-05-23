\name{as.cpdag.bnpostmcmc.list}
\alias{as.cpdag.bnpostmcmc.list}
\title{Convert 'bnpostmcmc list' to CPDAGs.}
\usage{\method{as.cpdag}{bnpostmcmc.list} (x, ...)}

\description{
  Convert 'bnpostmcmc list' to CPDAGs.
}

\details{
  Convert a full set of MCMC posterior samples to Completed
  Partially Directed Acyclic Graph (CPDAG). All MCMC runs
  are converted.
}
\value{A list containing a list of CPDAGs of class CPDAG.}
\arguments{
  \item{x}{An object of class bnpostmcmc.list}
  \item{...}{Further arguments (unused)}
}
