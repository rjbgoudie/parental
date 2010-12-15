\name{grobNodeLevelPlotSize}
\alias{grobNodeLevelPlotSize}
\title{Return the size of levelplot for node "node".}
\usage{grobNodeLevelPlotSize(node, parents, rawdata)}
\description{Return the size of levelplot for node "node". If the node has no 
parents, the size of a grobNodeName is returned instead.}
\value{A list of length 2 containing two items:
width: A object of class "unit"
height: A object of class "unit"}
\arguments{\item{node}{An integer of length 1, indicating which node the dimensions
should be computed for.}
\item{parents}{An object of class "parental" containing the graph that is to 
be plottedb}
\item{rawdata}{The rawdata}}
