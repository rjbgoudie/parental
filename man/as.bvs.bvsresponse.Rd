\name{as.bvs.bvsresponse}
\alias{as.bvs.bvsresponse}
\title{Convert 'bvsresponse' to 'bvs'.}
\usage{
  \method{as.bvs}{bvsresponse} (x, ...)
}
\arguments{
  \item{x}{An object of class 'bvsresponse'}

  \item{...}{further arguments (unused)}
}
\value{
  An object of class 'bvs'.
}
\description{
  Convert a 'bvsresponse' object to a 'bvs' object. A 'bvs'
  object is a full 'parental' object, whereas a
  'bvsresponse' object is smaller object designed
  specifically for fast MCMC iterations.
}

