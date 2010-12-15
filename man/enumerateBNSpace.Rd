\name{enumerateBNSpace}
\alias{enumerateBNSpace}
\title{Enumerate the space of BNs...}
\usage{enumerateBNSpace(n, allowCyclic=F, multicore=F)}
\description{Enumerate the space of BNs}
\details{Enumerate (ie make a list of, not just count) the entire space of 
Bayesian Networks models, on a given number of nodes.}
\value{A \code{parental.list} including ALL the directed acyclic 
graphs with \code{n} nodes.}
\arguments{\item{n}{The number of nodes}
\item{allowCyclic}{A logical indicating whether cyclic directed graphs
should be filtered out.}
\item{multicore}{A logical specifying whether to use 
\link[multicore]{mclapply}.}}
