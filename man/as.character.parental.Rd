\name{as.character.parental}
\alias{as.character.parental}
\title{Convert a parental object to a character vector...}
\usage{\method{as.character}{parental} (x, pretty=F, ...)}
\description{Convert a parental object to a character vector}
\details{Converts a parental object to a character representation.}
\value{An object of class "parental"}
\arguments{\item{x}{A character vector}
\item{pretty}{A logical, which determines the format of the output}
\item{...}{Further arguments (unused)}}
\examples{x <- parental(c(3), c(1), c(1, 2))
as.character(x)

x <- parental(c(3), c(1), c(1, 2))
as.character(x, pretty = TRUE)}
