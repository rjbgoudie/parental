\name{print.parental.list}
\alias{print.parental.list}
\title{Prints 'parental list'.}
\usage{\method{print}{parental.list} (x, ...)}
\description{Prints 'parental list'.}
\value{Prints the 'parental.list' object to the console.}
\seealso{\code{\link{parental.list}}}
\arguments{\item{x}{A 'parental.list' object}
\item{...}{Further arguments, currently unused.}}
\examples{p1 <- parental(c(), c(1), c(1, 2, 4), c(3))
p2 <- parental(c(2), c(), c(4), c(2, 3))
x <- parental.list(p1, p2)
x}
