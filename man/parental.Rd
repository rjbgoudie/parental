\name{parental}
\alias{parental}
\title{Create a parental object}
\description{
 Constructor function for a 'parental' object.
}
\usage{
parental(...)
}
\arguments{
 \item{...}{
 A series of vectors specifying the parents of each node. These
 vectors must be of storage.mode "integer".
 }
}
\examples{
parental(integer(0), 1)
}