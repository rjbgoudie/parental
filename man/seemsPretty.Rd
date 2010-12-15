\name{seemsPretty}
\alias{seemsPretty}
\title{A heuristic test for whether it appears that x is 'pretty-printed'.}
\usage{seemsPretty(x)}
\description{A heuristic test for whether it appears that x is 'pretty-printed'. ie 
whether it is of the form [2][][3] (printed-printed) or 
"c(2L), integer(0), c(3L)" (not pretty-printed)}
\value{A logical of length 1. Returns true if it appears x is 
pretty-printed.}
\arguments{\item{x}{A character vector defining a 'parental' object}}
