\name{combn2}
\alias{combn2}
\title{(Fast, simple) Generate All Combinations of n Elements, Taken m at a Time...}
\usage{combn2(x, m, required)}
\description{(Fast, simple) Generate All Combinations of n Elements, Taken m at a Time}
\details{A fast, simple version of \code{\link[utils]{combn}}.

ALSO NOTE. The output is SORTED!}
\arguments{\item{x}{The set of elements from which to choose. Unlike \code{combn}, 
if this is a single integer, only this single integer is used; 
\code{combn} uses \code{1:x} in this case.}
\item{m}{The size of the sets}
\item{required}{A numeric vector that is appended to each set}}
