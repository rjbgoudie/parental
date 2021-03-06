parental
==========

parental provides a very lightweight directed graph object and basic manipulation tools for [`R`][R], a free software environment for statistical computing and graphics. The emphasis is on representing Bayesian Networks, so the focus is on Directed Acyclic Graphs.

Basic operation
------------------------------
[View script as file](https://gist.github.com/970554)

A `bn` object is created by specifying the parents of each node.

``` r
x <- bn(c(2), c(), c(1, 2))
grplot(x)
```

Adjacency matrices can be converted to a `bn` using `as.bn`.

``` r
adj <- matrix(c(0, 0, 1, 1, 0, 0, 0, 0, 0), 3, 3)
x <- as.bn(adj)
grplot(x)
```

A `bn` object can be converted to an adjacency matrix

``` r
x <- bn(c(2), c(), c(1, 2))
as.adjacency(x)
```

Properties of a `bn`

``` r
x <- bn(c(2), c(), c(1, 2))
nNodes(x)
nEdges(x)
indegrees(x)
checkAcyclic(x)
topologicallyOrder(x)
```

Manipulating `bn` objects

``` r
x <- bn(c(2), c(), c(1, 2))
```

Fast, but less intuitive manipulation of `bn` objects

``` r
x <- bn(c(2), c(), c(1, 2))
x[[1]] <- c()
x
x[[2]] <- 1
x
```

Sample a BN

``` r
sampleBN(10)
sampleBN(10, maxNumberParents = 2)
```

An empty graph

``` r
empty(10, "bn")
```

Enumerate all the Bayesian Networks

``` r
enumerateBNSpace(3)
```

Tools for handling "routes matrices"

``` r
x <- bn(c(2), c(), c(1, 2))
routes(x)
```

Simulate from a `bn`

``` r
cpt <- list(
  as.table(array(c(0.7, 0.3), 2)), 
  as.table(array(c(0.5, 0.5, 0.2, 0.8), c(2, 2)))
)
net <- bn(c(), 1)
sim <- simulate(object = net, nsim = 1000, ptables = cpt)
```

Installation
------------
Download the current version, and `unzip` the file. Then install in `R` using the following, where `rjbgoudie-parental-XXXXX` is the name of the `unzip`ped directory/folder, and `path/to/rjbgoudie-parental-XXXXX` is the path to this folder.

``` r
install.packages("path/to/rjbgoudie-parental-XXXXX", repos = NULL, type = "source")
```

The package also depends on [`lattice`][cran:lattice], and `grid`, both of which are included with R.


How to cite
-----------
Goudie, R. J. B., & Mukherjee, S. (2016). A Gibbs Sampler for Learning DAGs. _Journal of Machine Learning Research_, **17**(30), 1-39. [http://jmlr.org/papers/v17/14-486.html](http://jmlr.org/papers/v17/14-486.html)


[R]: http://www.r-project.org "The R Project for Statistical Computing"
[cran:lattice]: http://cran.r-project.org/web/packages/lattice "lattice: Lattice Graphics"
