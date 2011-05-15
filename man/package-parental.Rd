\name{package-parental}
\title{The parental package.}
\description{The parental package.}
\details{The package \code{parental} provides lightweight tools for creating, 
modifying network/graph objects. The emphasis is on representing 
Bayesian Networks, so the focus is on Directed Acyclic Graphs.}
\docType{package}
\alias{package-parental}
\seealso{Constructors for objects: \code{\link{parental}}, 
\code{\link{bn}}, \code{\link{bvsresponse}}, \code{\link{bvs}}.
Particular graphs: \code{\link{complete}}, \code{\link{empty}}.
Plotting: \code{\link{grplot}}.
Properties: \code{\link{nNodes}}, \code{\link{nEdges}}, 
\code{\link{indegrees}}, \code{\link{checkAcyclic}}, 
\code{\link{topologicallyOrder}}, \code{\link{routes}}.
Collections of objects: \code{\link{parental.list}}, 
\code{\link{bn.list}}
Conversion to/from other graph objects: \code{\link{as.parental}}, 
\code{\link{as.bn}}.
Routes matrices: \code{\link{routes}}, \code{\link{routesAddEdge}}, 
\code{\link{routesRemoveEdge}}
Random Bayesian network: \code{\link{sampleBN}}
Draw data from a Bayesian network: \code{\link{simulate.bn}}}
\examples{# A `bn` object is created by specifying the parents of each node.
x <- bn(c(2), c(), c(1, 2))
grplot(x)

# A `bn` object can be converted to an adjacency matrix
adj <- matrix(c(0, 0, 1, 1, 0, 0, 0, 0, 0), 3, 3)
x <- as.bn(adj)
grplot(x)

# A `bn` object can be converted to an adjacency matrix
x <- bn(c(2), c(), c(1, 2))
as.adjacency(x)

# Properties of a `bn`
x <- bn(c(2), c(), c(1, 2))
nNodes(x)
nEdges(x)
indegrees(x)
checkAcyclic(x)
topologicallyOrder(x)

# Manipulating `bn` objects
x <- bn(c(2), c(), c(1, 2))

# Fast, but less intuitive manipulation of `bn` objects
x <- bn(c(2), c(), c(1, 2))
x[[1]] <- c()
x
x[[2]] <- 1
x

# Sample a BN
sampleBN(10)
sampleBN(10, maxNumberParents = 2)

# An empty graph
empty(10, "bn")

# Enumerate all the Bayesian Networks
enumerateBNSpace(3)

# Tools for handling "routes matrices"
x <- bn(c(2), c(), c(1, 2))
routes(x)

# Simulate from a `bn`
cpt <- list(
as.table(array(c(0.7, 0.3), 2)), 
as.table(array(c(0.5, 0.5, 0.2, 0.8), c(2, 2)))
)
net <- bn(c(), 1)
sim <- simulate(object = net, nsim = 1000, ptables = cpt)}
