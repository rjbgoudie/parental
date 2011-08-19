\name{prepanel.parental}
\alias{prepanel.parental}
\title{Pre-panel function for plotting parental objects.}
\usage{
  prepanel.parental(x, y, parents, rawdata = NULL,
  grobNodeSize, offset, islist = F, widthMultiplier = 2,
  heightMultipler = 1)
}
\arguments{
  \item{x}{The x-coordinates of the nodes}

  \item{y}{The y-coordinates of the nodes}

  \item{parents}{An object of class 'parental'}

  \item{rawdata}{A data frame containing the raw data}

  \item{grobNodeSize}{A function that returns the size of a
  node grob}

  \item{offset}{An offset}

  \item{islist}{A logical}

  \item{widthMultiplier}{A width multiplier}

  \item{heightMultipler}{A height multiplier}
}
\value{
  A list of length two. This contains two components:
  \code{xlim} and \code{ylim}, each of which contain the
  max and min values in that dimension.
}
\description{
  Computes the bounds of the final plot.
}
\seealso{
  \code{\link{grplot}}
}

