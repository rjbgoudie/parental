\name{as.cpdag.bnpostmcmc.list}
\alias{as.cpdag.bnpostmcmc.list}
\title{Convert 'bnpostmcmc list' to CPDAGs.}
\usage{
  \method{as.cpdag}{bnpostmcmc.list} (x, ...)
}
\arguments{
  \item{x}{An object of class bnpostmcmc.list}

  \item{...}{Further arguments (unused)}
}
\value{
  A list containing a list of CPDAGs of class CPDAG.
}
\description{
  Convert a full set of MCMC posterior samples to Completed
  Partially Directed Acyclic Graph (CPDAG). All MCMC runs
  are converted.
}

