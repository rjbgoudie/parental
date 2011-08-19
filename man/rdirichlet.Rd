\name{rdirichlet}
\alias{rdirichlet}
\title{Sampling from Dirichlet distribution.}
\usage{
  rdirichlet(n, alpha)
}
\arguments{
  \item{n}{Number of random vectors to generate.}

  \item{alpha}{Vector containing shape parameters.}
}
\value{
  returns a matrix with n rows, each containing a single
  Dirichlet random deviate.
}
\description{
  Generate random deviates from the Dirichlet distribution.
}
\examples{
rdirichlet(20, c(1,1,1)
}
\author{
  Code original posted by Ben Bolker to R-News on Fri Dec
  15 2000. See
  \url{http://www.r-project.org/nocvs/mail/r-help/2000/3865.html}.
  Ben attributed the code to Ian Wilson <i.wilson
}

