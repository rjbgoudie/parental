\name{as.character.parental.list}
\alias{as.character.parental.list}
\title{Convert a 'parental list' to a character vector.}
\usage{\method{as.character}{parental.list} (x, pretty=F, ...)}
\description{Convert a 'parental list' to a character vector.}
\value{A character vector.}
\arguments{\item{x}{A object of class 'parental.list'. A list of \code{parental} 
objects}
\item{pretty}{A logical of length 1 indicating if the character vectors 
are pretty-printed.}
\item{...}{Further arguments (unused)}}
\examples{p1 <- parental(c(), c(1), c(1, 2, 4), c(3))
p2 <- parental(c(2), c(), c(4), c(2, 3))
x <- parental.list(p1, p2)
as.character(x)}
