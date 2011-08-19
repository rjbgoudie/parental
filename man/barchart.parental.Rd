\name{barchart.parental}
\alias{barchart.parental}
\title{Barchart of a graph.}
\usage{
  \method{barchart}{parental} (x, data, type =
  c("indegrees", "outdegrees"), ...)
}
\arguments{
  \item{x}{An object of class parental}

  \item{data}{currently unused}

  \item{type}{A character vector of length one. One of
  \code{indegree}, \code{outdegree}}

  \item{...}{Further arguments, passed onto
  \code{barchart()}}
}
\value{
  A barchart
}
\description{
  Plot a barchart of summary statistics about a 'parental'
  object.
}

