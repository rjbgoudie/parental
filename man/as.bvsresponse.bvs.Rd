\name{as.bvsresponse.bvs}
\alias{as.bvsresponse.bvs}
\title{Convert a 'bvs' object to a 'bvsresponse' object.}
\usage{as.bvsresponse.bvs(x, response)}
\description{Convert a 'bvs' object to a 'bvsresponse' object.}
\details{A 'bvs' object is a full 'parental' object, whereas a 'bvsresponse' 
object is smaller object designed specifically for fast MCMC iterations.}
\value{An object of class 'bvsresponse'.}
\arguments{\item{x}{An object of class 'bvsresponse'}
\item{response}{An integer of length 1 specifying which node is 
the response. This is not required, except for the empty graph.}}
