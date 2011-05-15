\name{filterCyclic}
\alias{filterCyclic}
\title{Filter cyclic graphs.}
\usage{filterCyclic(bnlist)}
\description{Filter cyclic graphs.}
\details{Removes the cyclic graphs from a list of graphs}
\value{An object of class \code{bn.list}, containing all the acyclic 
graphs in \code{bnlist}}
\seealso{\code{\link{enumerateBNSpace}}}
\arguments{\item{bnlist}{A \code{parental.list} of graphs}}
\examples{x <- enumerateBNSpace(3, allowCyclic = TRUE)
filterCyclic(x)}
