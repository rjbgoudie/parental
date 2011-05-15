\name{as.cpdag0.bnpostmcmc.list}
\alias{as.cpdag0.bnpostmcmc.list}
\title{Convert 'bnpostmcmc list' to CPDAG (old version).}
\usage{\method{as.cpdag0}{bnpostmcmc.list} (x, ...)}
\description{Convert 'bnpostmcmc list' to CPDAG (old version).}
\details{Convert a full set of MCMC posterior samples to 
Completed Partially Directed Acyclic Graph (CPDAG).
All MCMC runs are converted.}
\value{A list containing a list of CPDAGs of class CPDAG.}
\arguments{\item{x}{An object of class bnpostmcmc.list}
\item{...}{Further arguments (unused)}}
