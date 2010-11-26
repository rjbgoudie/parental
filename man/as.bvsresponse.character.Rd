\name{as.bvsresponse.character}
\alias{as.bvsresponse.character}
\title{Convert a character vector (a comma-separated string) specifying the...}
\usage{as.bvsresponse.character(x, response, nNodes)}
\description{Convert a character vector (a comma-separated string) specifying the 
parents of a response to a 'bvsresponse' object.}
\details{For example, as.bvsresponse("1,2,3", 4, 5) returns a 'bvsresponse' object 
with 5 nodes, with node 4 as response, and nodes 1, 2 and 3 as its 
parents.}
\value{If length(x) == 1:
An object of class 'bvsresponse'.
If length(x) > 1:
A list of class "bvsresponse.list" containing a number of 
'bvsresponse' objects}
\arguments{\item{x}{A character string (possibly vector), a comma-separated list of 
the parents of the response.}
\item{response}{An integer of length 1 specifying which node 
is the response.}
\item{nNodes}{An integer of length 1 specifying the number of nodes in the 
variable selection.}}
