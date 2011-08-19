\name{as.bvsresponse.bvs}
\alias{as.bvsresponse.bvs}
\title{Convert a 'bvs' object to a 'bvsresponse' object.}
\usage{
  \method{as.bvsresponse}{bvs} (x, response, ...)
}
\arguments{
  \item{x}{An object of class 'bvsresponse'}

  \item{response}{An integer of length 1 specifying which
  node is the response. This is not required, except for
  the empty graph.}

  \item{...}{Further arguments (unused)}
}
\value{
  An object of class 'bvsresponse'.
}
\description{
  A 'bvs' object is a full 'parental' object, whereas a
  'bvsresponse' object is smaller object designed
  specifically for fast MCMC iterations.
}

