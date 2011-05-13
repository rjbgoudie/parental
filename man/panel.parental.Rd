\name{panel.parental}
\alias{panel.parental}
\title{Panel function for ploting a parental graph...}
\usage{panel.parental(x, y, parents, layout, col, alpha, edgecol, edgealpha,
    islist=F, rawdata, grobNode, grobNodeSize, offset,
    widthMultiplier=2, heightMultipler=3, ...)}
\description{Panel function for ploting a parental graph}
\details{Panel function for grplot.}
\value{A panel}
\arguments{\item{x}{The x-coordinates of the nodes}
\item{y}{The y-coordinates of the nodes}
\item{parents}{An object of class parental}
\item{layout}{The layout (not currently used??)}
\item{col}{A vector of colours for the nodes (not currently used??)}
\item{alpha}{A vector of alpha values for the nodes (not currently used??)}
\item{edgecol}{A matrix of edge colours}
\item{edgealpha}{A matrix of edge alpha values}
\item{islist}{A logical of length 1}
\item{rawdata}{A data frame of raw data.}
\item{grobNode}{A function that returns the grob for a given node}
\item{grobNodeSize}{A function that returns the size of a grob for a 
given node}
\item{offset}{An offset}
\item{widthMultiplier}{A width multiplier}
\item{heightMultipler}{A height multiplier}
\item{...}{Further arguments}}
