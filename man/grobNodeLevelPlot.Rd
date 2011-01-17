\name{grobNodeLevelPlot}
\alias{grobNodeLevelPlot}
\title{Return a grob for node "node" when a levelplot is plotted on each node...}
\usage{grobNodeLevelPlot(node, parents, rawdata, strip.lines=20,
    strip.left.lines=15, theme=grobNodeLevelPlotDefaultTheme(),
    horizontal=F, ...)}
\description{Return a grob for node "node" when a levelplot is plotted on each node}
\value{A "grob"}
\arguments{\item{node}{An integer of length 1, indicating which node the dimensions
should be computed for.}
\item{parents}{An object of class "parental" containing the graph that 
is to be plotted}
\item{rawdata}{The rawdata}
\item{strip.lines}{...}
\item{strip.left.lines}{...}
\item{theme}{...}
\item{horizontal}{...}
\item{...}{...}}
