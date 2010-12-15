\name{routesAddEdge}
\alias{routesAddEdge}
\title{Update a routes matrix (edge addition)...}
\usage{routesAddEdge(x, i, j)}
\description{Update a routes matrix (edge addition)}
\details{A routes matrix is a matrix A, such that each element (i, j) is the number 
of routes from i to j in some directed graph.

This function updates the routes matrix to account for the addition of an 
edge from i to j in the directed graph}
\arguments{\item{x}{A routes matrix}
\item{i}{The node from which the added edge emanates}
\item{j}{The node that the added edge goes to}}
