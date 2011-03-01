\name{is.valid.bn}
\alias{is.valid.bn}
\title{Checks whether the supplied 'bn is valid.}
\usage{\method{is.valid}{bn} (x)}
\description{Checks whether the supplied 'bn is valid.
Tests that the parents are sorted correctly, are of 
storage.mode() == "integer".
Additionally checks that the supplied bn is acyclic.}
\value{A logical of length 1 indicating whether x is a valid 'bn' object}
\arguments{\item{x}{A object of class 'bn'}}
