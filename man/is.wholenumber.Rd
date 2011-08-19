\name{is.wholenumber}
\alias{is.wholenumber}
\title{Tests for whole numbers.}
\usage{
  is.wholenumber(x, tol = .Machine$double.eps^0.5)
}
\arguments{
  \item{x}{A numeric vector of length 1, which the function
  check if it is whole}

  \item{tol}{A numeric vector of length 1 specifying the
  tolerance allowed}
}
\value{
  A logical vector of length 1, taking the value TRUE is x
  is a whole number and false otherwise
}
\description{
  Tests if x is a whole number, up to the tolerance
  specified by tol. Taken from an example on the help page
  for \code{\link{is.integer}}.
}
\details{
  Note that is.integer(x) does not test if x contains
  integer numbers!
}
\examples{
is.wholenumber(2)
is.wholenumber(2.1)
}
\seealso{
  \code{\link{is.integer}}
}

