\name{grplot.parental.list}
\alias{grplot.parental.list}
\title{Plot a parental...}
\usage{grplot.parental.list(parentallist, col=1, alpha=1, edgecol=1,
    edgealpha=1, layout.par=list(niter = 2e+05), grobNode=grobNodeName,
    grobNodeSize=grobNodeNameSize, offset=0.25, ...)}
\description{Plot a parental.list}
\details{Plots a parental graph, by default using the layout routines of the 
package 'network'.}
\value{A lattice plot}
\arguments{\item{parentallist}{An object of class 'parental.list'}
\item{col}{A vector of colours for the nodes (FIXME - does not currently 
work.)}
\item{alpha}{A vector of alpha values for the nodes (FIXME - does not 
currently work.)}
\item{edgecol}{A matrix of edge colours.}
\item{edgealpha}{A matrix of edge alpha values}
\item{layout.par}{Passed to 
\code{\link[network]{network.layout.fruchtermanreingold}}}
\item{grobNode}{A grob function that will be used to draw the nodes}
\item{grobNodeSize}{A function that can compute the sizes of the nodes}
\item{offset}{A offset}
\item{...}{Further arguments (not currently passed on?)}}
