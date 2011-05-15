\name{is.valid.bn}
\alias{is.valid.bn}
\title{Tests a 'bn' for validity...}
\usage{\method{is.valid}{bn} (x)}
\description{Tests a 'bn' for validity}
\details{Checks whether the supplied 'bn is valid.
Tests that the parents are sorted correctly, are of 
storage.mode() == "integer".
Additionally checks that the supplied bn is acyclic.}
\value{A logical of length 1 indicating whether x is a valid 'bn' object}
\seealso{\code{\link{is.valid.parental}}}
\arguments{\item{x}{A object of class 'bn'}}
\examples{x <- bn(c(), 1, 2)
is.valid(x)}
