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

Properties of a `bn`

``` r
x <- bn(c(2), c(), c(1, 2))
nNodes(x)
nEdges(x)
indegrees(x)
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


Installation
------------


Contact
-------


[R]: http://www.r-project.org "The R Project for Statistical Computing"
