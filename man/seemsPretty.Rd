\name{seemsPretty}
\alias{seemsPretty}
\title{Heuristic test for pretty-printing.}
\usage{seemsPretty(x)}
\description{Heuristic test for pretty-printing.}
\details{#' A heuristic test for whether it appears that x is 'pretty-printed'. ie 
whether it is of the form [2][][3] (printed-printed) or 
"c(2L), integer(0), c(3L)" (not pretty-printed)}
\value{A logical of length 1. Returns true if it appears x is 
pretty-printed.}
\arguments{\item{x}{A character vector defining a 'parental' object}}
\examples{seemsPretty("integer(0),1,c(1,2,4),3")
seemsPretty("[][1][2]")}
