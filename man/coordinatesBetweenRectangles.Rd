\name{coordinatesBetweenRectangles}
\alias{coordinatesBetweenRectangles}
\title{Calculate coordinates of line between bounding boxes...}
\usage{coordinatesBetweenRectangles(x1, y1, x2, y2, w1, h1, w2, h2)}
\description{Calculate coordinates of line between bounding boxes}
\details{We have points \code{(x1, y1)} and \code{(x2, y2)} in Cartesian space. 
We wish to draw a line between these two points.

However, these two points have rectangles centred around them.
The first point has a rectangle width \code{w1} and height \code{h1} drawn 
around it; the second has a rectangle with \code{w2} and height \code{h2}
drawn around it. Note that the vertices of the first rectangle are 
\code{(x1-w1/2, x1+h1/2)}, \code{(x1+w1/2, x1+h1/2)},
\code{(x1+w1/2, x1-h1/2)} \code{(x1-w1/2, x1-h1/2)}.

This function returns the coordinates of the points on which the 
line drawn between the two points intersects with each bounding box.}
\value{A numeric vector of length 4. The first two are the x- and 
y-coordinates on the bounding box of \code{(x1, y1)}. The 
third and fourth are the x- and y-coordinates on the bounding box of 
\code{(x2, y2)}}
\arguments{\item{x1}{The x-coordinate of the first point}
\item{y1}{The y-coordinate of the first point}
\item{x2}{The x-coordinate of the second point}
\item{y2}{The y-coordinate of the second point}
\item{w1}{The width of the bounding box around \code{(x1, y1)}}
\item{h1}{The height of the bounding box around \code{(x1, y1)}}
\item{w2}{The width of the bounding box around \code{(x2, y2)}}
\item{h2}{The height of the bounding box around \code{(x1, y1)}}}
