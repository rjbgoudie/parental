\name{c.parental.list}
\alias{c.parental.list}
\title{Concatenates 'parental list'.}
\usage{
  \method{c}{parental.list} (...)
}
\arguments{
  \item{...}{Any number of 'parental.list' objects}
}
\value{
  An new 'parental.list' object, including all the supplied
  parental.lists
}
\description{
  Concatenates 'parental list'.
}
\examples{
p1 <- parental(c(), c(1), c(1, 2, 4), c(3))
p2 <- parental(c(2), c(), c(4), c(2, 3))
x <- parental.list(p1, p2)
y <- parental.list(p1, p1)
c(x, y)
}
\seealso{
  \code{\link{parental.list}},
  \code{\link{[.parental.list}}
}

