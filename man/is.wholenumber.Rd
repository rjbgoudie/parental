\name{is.wholenumber}
\alias{is.wholenumber}
\title{Tests if x is a whole number, up to the tolerance specified by tol.}
\usage{is.wholenumber(x, tol=.Machine$double.eps^0.5)
}
\description{Tests if x is a whole number, up to the tolerance specified by tol.
Taken from an example on the help page for is.integer()}
\value{A logical vector of length 1, taking the value TRUE is x is a whole 
number and false otherwise}
\arguments{\item{x}{A numeric vector of length 1, which the function check if it is 
whole}
\item{tol}{A numeric vector of length 1 specifying the tolerance allowed}
}

