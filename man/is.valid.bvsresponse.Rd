\name{is.valid.bvsresponse}
\alias{is.valid.bvsresponse}
\title{Checks whether the supplied 'bvsresponse' is valid.}
\usage{is.valid.bvsresponse(x)}
\description{Checks whether the supplied 'bvsresponse' is valid.
Tests that the parents are sorted correctly, are of 
storage.mode() == "integer".
Additionally tests the parents all exist.}
\value{A logical of length 1 indicating whether x is a valid 
'bvsresponse' object}
\arguments{\item{x}{A object of class 'bvsresponse'}}
