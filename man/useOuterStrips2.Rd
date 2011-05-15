\name{useOuterStrips2}
\alias{useOuterStrips2}
\title{Put Strips on the Boundary of a Lattice Display.}
\usage{useOuterStrips2(x, strip=strip.default2,
    strip.left=strip.custom2(horizontal = horizontal), top=1,
    strip.lines=1, strip.left.lines=strip.lines, horizontal=F)}
\description{Put Strips on the Boundary of a Lattice Display.}
\details{Try to update a "trellis" object so that strips are only shown on the 
top and left boundaries when printed, instead of in every panel as is 
usual. This version extend the version in LatticeExtra to allow more than 
two conditioning variables.}
\value{An object of class "trellis", essentially the same as x, but with 
certain properties modified.}
\arguments{\item{x}{An object of class "trellis".}
\item{strip}{A function, character string, or logical that would 
be appropriate strip and strip.left arguments respectively in a high 
level lattice function call (see xyplot). Note, however, that the strip 
function must be of the form of strip.default2. 
The equivalent of strip.custom here is strip.custom2.}
\item{strip.left}{As strip, for the strips down the left.}
\item{top}{Determines the number of strips that are drawn along the top 
of the plot. Conditioning variables 1, ..., top will be displayed along 
the top of the plot (using strip), and variables top + 1, ..., dim(x) 
will be displayed along the left hand side of the plot.}
\item{strip.lines}{height of strips in number of lines; helpful for 
multi-line text or mathematical annotation in strips.}
\item{strip.left.lines}{As strip.lines, for strips down the left.}
\item{horizontal}{...}}
