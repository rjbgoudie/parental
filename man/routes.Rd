\name{routes}
\alias{routes}
\title{Returns a matrix encoding the number of routes between the nodes of the...}
\usage{routes(x)}
\description{Returns a matrix encoding the number of routes between the nodes of the 
bn x.}
\details{Element (i, j) contains the number of routes from node i to node j
for i != j
Element (i, i) contains 1 for all i.}
\value{A matrix of dimension nNodes(x) x nNodes(x)}
\arguments{\item{x}{An object of class 'bn'.}}
