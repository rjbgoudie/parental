\name{simulate.bn}
\alias{simulate.bn}
\title{Draw data according to a Bayesian Network.}
\usage{simulate.bn(object, nsim, seed, ptables, expectation=F, ...)}
\description{Draw data according to a Bayesian Network.}
\seealso{\code{\link{cptBinary}}}
\arguments{\item{object}{A object of class \code{\link{bn}}}
\item{nsim}{Number of simulations}
\item{seed}{The starting seed (UNIMPLEMENTED)}
\item{ptables}{...}
\item{expectation}{...}
\item{...}{Further arguments, currently unused}}
\examples{cpt <- list(
as.table(array(c(0.7, 0.3), 2)), 
as.table(array(c(0.5, 0.5, 0.2, 0.8), c(2, 2)))
)
net <- bn(NULL, 1)
sim <- simulate(object = net, nsim = 1000, ptables = cpt)

# a three node example
cpt <- list(
as.table(array(c(0.7, 0.3), 2)), 
as.table(array(c(0.5, 0.5,
0.2, 0.8),
c(2, 2))),
as.table(array(c(
# prob of 1 then 2 given
0.8, 0.2, # p1 = 1, p2 = 1
0.2, 0.8, # p1 = 2, p2 = 1
0.2, 0.8, # p1 = 1, p2 = 2
0.2, 0.8  # p1 = 2, p2 = 2
), c(2, 2, 2)))
)
net <- bn(NULL, 1L, c(1L, 2L))
sim <- simulate(object = net, nsim = 1000, ptables = cpt)}
