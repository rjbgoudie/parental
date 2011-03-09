\name{combn3}
\alias{combn3}
\title{(C version) Generate All Combinations of n Elements, Taken m at a Time...}
\usage{combn3(x, m, required=integer(0))}
\description{(C version) Generate All Combinations of n Elements, Taken m at a Time}
\details{A fast, simple version of \code{\link[utils]{combn}}.

ALSO NOTE. The output is SORTED!

If \code{x} is of length 0, then a blank list \code{list()} is returned,
unless \code{required} is of length great than 0, in which case,
list(required) is returned.}
\value{A list of the combinations}
\arguments{\item{x}{The set of elements from which to choose. Unlike \code{combn}, 
if this is a single integer, only this single integer is used; 
\code{combn} uses \code{1:x} in this case. (This is also unlike the
version in \code{gRbase}.}
\item{m}{The size of the sets}
\item{required}{A numeric vector that is appended to each set.
The length of required must be positive!}}
