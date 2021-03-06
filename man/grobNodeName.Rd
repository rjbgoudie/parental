\name{grobNodeName}
\alias{grobNodeName}
\title{Return a grob for node "node" when the name of the node is plotted.}
\usage{
  grobNodeName(node, parents, rawdata = NULL, gp)
}
\arguments{
  \item{node}{An integer of length 1, indicating which node
  the dimensions should be computed for.}

  \item{parents}{An object of class "parental" containing
  the graph that is to be plotted}

  \item{rawdata}{The rawdata}

  \item{gp}{Graphical parameters, passed to
  \code{\link[grid]{textGrob}}.}
}
\value{
  A "grob"
}
\description{
  Return a grob for node "node" when the name of the node
  is plotted.
}
\seealso{
  \code{\link{grplot}}
}

