\name{grplot.parental}
\alias{grplot.parental}
\title{Plot a graph.}
\usage{\method{grplot}{parental} (parents, col=1, alpha=1,
    edgecol, edgealpha=1, layout.par=list(niter = 1e+05),
    grobNode=grobNodeName, grobNodeSize=grobNodeNameSize,
    offset=0.25, hideIsolates=F, layout, ...)}

\description{
  Plot a graph.
}

\details{
  Plots a parental graph, by default using the layout
  routines of the package 'network'.
}
\value{A lattice plot}
\seealso{\code{\link{grplot.parental.list}}, \code{\link{panel.parental}}}
\arguments{
  \item{parents}{An object of class 'parental'}
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
  \item{hideIsolates}{A logical of length 1. If true, isolates nodes 
(those not connected to any other node) are removed.}
  \item{layout}{Optionally provide the coordinates at which each node will 
be drawn. This should be supplied as a data.frame with columns 
\code{xcoord} and \code{ycoord}.}
  \item{...}{Further arguments (not currently passed on?)}
}
\examples{x <- parental(c(), c(1), c(2))
grplot(x)}
