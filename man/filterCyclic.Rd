\name{filterCyclic}
\alias{filterCyclic}
\title{Filter cyclic graphs.}
\usage{
  filterCyclic(bnlist)
}
\arguments{
  \item{bnlist}{A \code{parental.list} of graphs}
}
\value{
  An object of class \code{bn.list}, containing all the
  acyclic graphs in \code{bnlist}
}
\description{
  Removes the cyclic graphs from a list of graphs
}
\examples{
x <- enumerateBNSpace(3, allowCyclic = TRUE)
filterCyclic(x)
}
\seealso{
  \code{\link{enumerateBNSpace}}
}

