\name{as.character.bvsresponse}
\alias{as.character.bvsresponse}
\title{Convert a 'bvsresponse' object to a character string.}
\usage{as.character.bvsresponse(x, ...)}
\description{Convert a 'bvsresponse' object to a character string. Note that the 
number of nodes, and which node the response is not contained in the 
resulting character string.}
\details{For example, as.character() on a bvsresponse with 10 nodes, response 
node 5, which has parents 2, 3, 6, and 10 returns "2,3,6,10".}
\value{An object of class 'parental'.}
\arguments{\item{x}{An object of class 'bvsresponse'}
\item{...}{Further arguments (unused)}}
