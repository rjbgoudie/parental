\name{notdiag}
\alias{notdiag}
\title{Retrieve off-diagonal elements...}
\usage{notdiag(mx)}
\description{Retrieve off-diagonal elements}
\details{Retrieve off-diagonal elements}
\value{A vector of off-diagonals}
\seealso{\code{\link{diag}}, \code{\link{lower.tri}}, 
\code{\link{upper.tri}}}
\arguments{\item{mx}{A matrix}}
\examples{m <- matrix(1:9, 3, 3)
notdiag(m)}
