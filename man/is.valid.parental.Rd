\name{is.valid.parental}
\alias{is.valid.parental}
\title{Check validity.}
\usage{\method{is.valid}{parental} (x)}

\description{
  Check validity.
}

\details{
  Checks whether the supplied \code{parental} is valid.
  Tests that the parents are sorted correctly, are of
  storage.mode() == "integer"
}
\value{A logical of length 1 indicating whether x is a valid 
\code{parental} object}
\seealso{\code{\link{is.valid.bn}}}
\arguments{
  \item{x}{A object of class \code{parental}}
}
\examples{x <- parental(c(), c(1), c(1, 2, 4), c(3))
is.valid(x)}
