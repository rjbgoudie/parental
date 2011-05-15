\name{bn}
\alias{bn}
\title{Constructor function for a 'bn' object.}
\usage{bn(...)}
\description{Constructor function for a 'bn' object.}
\details{\code{bn} is a subclass of \code{parental}. See \code{\link{parental}} 
for detailed documentation.}
\value{An object of class 'bn'.}
\seealso{\code{\link{parental}}}
\arguments{\item{...}{A series of vectors specifying the parents of each node. 
These vectors must be of storage.mode "integer".}}
\examples{bn(c(), 1, 2)}
