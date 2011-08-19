\name{as.parental.character}
\alias{as.parental.character}
\title{Convert a character string to a parental.}
\usage{
  \method{as.parental}{character} (x, pretty = F, ...)
}
\arguments{
  \item{x}{An character object}

  \item{pretty}{A logical indicating whether the supplied
  object is pretty}

  \item{...}{Further arguments (unused)}
}
\value{
  A \code{parental} object.
}
\description{
  Format required is one of two. Either standard or pretty.
  See examples.
}
\examples{
x <- "[3][1][2][1][1,3]"
as.parental(x, pretty = TRUE)

x <- "3,1,2,1,c(1,3)"
as.parental(x)
}

