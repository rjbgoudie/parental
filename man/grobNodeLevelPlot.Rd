\name{grobNodeLevelPlot}
\alias{grobNodeLevelPlot}
\title{Levelplot node grob.}
\usage{
  grobNodeLevelPlot(node, parents, rawdata, strip.lines =
  20, strip.left.lines = 15, theme =
  grobNodeLevelPlotDefaultTheme(), horizontal = F, ...)
}
\arguments{
  \item{node}{An integer of length 1, indicating which node
  the dimensions should be computed for.}

  \item{parents}{An object of class "parental" containing
  the graph that is to be plotted}

  \item{rawdata}{The rawdata}

  \item{strip.lines}{...}

  \item{strip.left.lines}{...}

  \item{theme}{...}

  \item{horizontal}{...}

  \item{...}{...}
}
\value{
  A "grob"
}
\description{
  Return a grob for node "node" when a levelplot is plotted
  on each node
}
\seealso{
  \code{\link{grplot}}
}

