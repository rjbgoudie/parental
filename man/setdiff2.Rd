\name{setdiff2}
\alias{setdiff2}
\title{A dual-direction, fast (and dangerous!) version of setdiff.}
\usage{setdiff2(x, y)
}
\description{A dual-direction, fast (and dangerous!) version of setdiff.}
\details{setdiff() is not symmetric. This function returns a list with component 1 
equivalent to setdiff(x, y) and component 2 equivalent to setdiff(y, x).

Note that unlike setdiff() THE OUTPUT MAY CONTAIN DUPLICATES. 
eg     setdiff2(c(1,1,2), c(2, 3))[[1]] == c(1, 1)
BUT setdiff(c(1, 1, 2), c(2, 3)) == 1

Additionally the inputs are NOT coerced to vectors, again, unlike setdiff}
\value{A list of length 2, with setdiff(x, y) in component 1 and 
setdiff(y, x) in component 2 (apart from the differences described above).}
\arguments{\item{x}{A numeric vector}
\item{y}{A numeric vector}
}

