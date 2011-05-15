\name{as.parental.character}
\alias{as.parental.character}
\title{Convert a character string to a parental...}
\usage{\method{as.parental}{character} (x, pretty=F, ...)}
\description{Convert a character string to a parental}
\details{Format required is one of two. Either standard or pretty. See examples.}
\value{A \code{parental} object.}
\arguments{\item{x}{An character object}
\item{pretty}{A logical indicating whether the supplied object is pretty}
\item{...}{Further arguments (unused)}}
\examples{x <- "[3][1][2][1][1,3]"
as.parental(x, pretty = T)

x <- "3,1,2,1,c(1,3)"
as.parental(x)}
