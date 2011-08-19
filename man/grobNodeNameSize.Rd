\name{grobNodeNameSize}
\alias{grobNodeNameSize}
\title{Return the size of node when the name of the node is plotted.}
\usage{
  grobNodeNameSize(node, parents, rawdata = NULL)
}
\arguments{
  \item{node}{An integer of length 1, indicating which node
  the dimensions should be computed for.}

  \item{parents}{An object of class "parental" containing
  the graph that is to be plotted}

  \item{rawdata}{The rawdata}
}
\value{
  A list of length 2 containing two items: width: An
  integer vector of length 1. The width in characters.
  height: An integer vector of length 1. The height in
  lines.
}
\description{
  This returns size of the label. Specifically, the return
  width is number of characters in the label.
}
\details{
  The height is the number of newlines.
}
\seealso{
  \code{\link{grplot}}
}

