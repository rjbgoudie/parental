\name{as.parental.bn}
\alias{as.parental.bn}
\title{Convert a 'bn' to a 'parental'.}
\usage{
  \method{as.parental}{bn} (x, ...)
}
\arguments{
  \item{x}{An object of class 'bn'}

  \item{...}{Further arguments (unused)}
}
\value{
  The object 'x', but of class 'parental'
}
\description{
  Convert a 'bn' to a 'parental'.
}
\examples{
x <- bn(c(), c(1), c(1, 2))
as.parental(x)
}

