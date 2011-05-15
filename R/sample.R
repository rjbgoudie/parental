# Part of the "parental" package, http://github.com/rjbgoudie/parental
# 
# This software is distributed under the GPL-3 license.  It is free,
# open source, and has the attribution requirements (GPL Section 7) in
#   http://github.com/rjbgoudie/parental
# 
# Note that it is required that attributions are retained with each function.
#
# Copyright 2008 Robert J. B. Goudie, University of Warwick

#' Draw a 'random' BN
#' 
#' Generates a BN, by choosing an order, then sampling a BN that respects 
#' that order
#' 
#' @param n The number of nodes for the Bayesian network. An integer.
#' @param maxNumberParents The maximum indegree of the network
#' @return A new \code{\link{bn}}.
#' @export
#' @seealso An alternative \code{\link{sampleBN2}}
#' @examples
#' sampleBN(5)
#' sampleBN(10)
#' sampleBN(10, 2)
sampleBN <- function(n, maxNumberParents = NULL){
  nodeSeq <- seq_len(n)
  
  # sample an order
  ordering <- sample(nodeSeq, replace = F)
  
  out <- lapply(nodeSeq, function(i){
    rank <- which(ordering == i)
    if (rank > 1){
      potentialParents <- ordering[seq_len(rank - 1)]
      numberOfPotentialParents <- length(potentialParents)
      
      if (!is.null(maxNumberParents) && 
          numberOfPotentialParents > maxNumberParents){
        numberOfPotentialParents <- maxNumberParents
      }
      
      numberOfParents <- sample.int(numberOfPotentialParents + 1, size = 1)
      numberOfParents <- numberOfParents - 1
      
      if (numberOfPotentialParents > 1){
        out <- sort.int(sample(potentialParents,
                               size = numberOfParents,
                               replace = F))
        storage.mode(out) <- "integer"
        out
      }
      else if (numberOfPotentialParents == 1){
        out <- sort.int(potentialParents[sample(c(T, F),
                                                size = numberOfParents)])
        storage.mode(out) <- "integer"
        out
      }
      else {
        stop("Unexpected")
      }
    }
    else {
      integer(0)
    }
  })
  class(out) <- c("bn", "parental")
  out
}

#' Draw a 'random' BN
#' 
#' NOT currently working?
#' 
#' @param n The number of nodes
#' @param k The dim of the hypercube.
#' @seealso \code{\link{sampleBN}}
#' @export
#' @return A BN
sampleBN2 <- function(n, k){
  # hypercube
  # the higher k the sparser the partial order
  
  x <- vapply(seq_len(n), function(x) runif(k), numeric(k))
  out <- empty(n, class = "bn")
  for (i in seq_len(n)){
    for (j in setdiff(seq_len(n), i)){
      # an arrow i -> j  if for all k, i < j
      
      if (k == 1){
        if (all(x[i] < x[j])){
          out[[j]] <- sort.int(c(out[[j]], i))
        }
      } else {
        if (all(x[, i] < x[, j])){
          out[[j]] <- sort.int(c(out[[j]], i))
        }
      }
    }
  }
  out
}

#' ...
#' 
#' ....
#' @param n ...
#' @param size ...
#' @param replace ...
#' @param prob ...
#' @export
#' @export
expected.sample <- function(n, size, replace, prob){
  exp <- prob * size
  out <- rep(seq.int(n), exp)
  shortage <- size - length(out)
  if (shortage > 0){
    shortfall <- exp - floor(exp)
    whichGreatestShortfall <- order(shortfall, decreasing = T)[1]
    out <- c(rep(whichGreatestShortfall, shortage), out)
    out <- sort.int(out)
  }
  out
}

##### sensitivity to the order in which parents are specified
#' Draw data according to a Bayesian Network
#' 
#' 
#' @param object A object of class \code{\link{bn}}
#' @param nsim Number of simulations
#' @param seed The starting seed (UNIMPLEMENTED)
#' @param ptables ...
#' @param expectation ...
#' @param ... Further arguments, currently unused
#' @export
#' @seealso \code{\link{cptBinary}}
#' @examples
#' cpt <- list(
#'   as.table(array(c(0.7, 0.3), 2)), 
#'   as.table(array(c(0.5, 0.5, 0.2, 0.8), c(2, 2)))
#' )
#' net <- bn(NULL, 1)
#' sim <- simulate(object = net, nsim = 1000, ptables = cpt)
#' 
#' # a three node example
#' cpt <- list(
#'   as.table(array(c(0.7, 0.3), 2)), 
#'   as.table(array(c(0.5, 0.5,
#'                    0.2, 0.8),
#'                  c(2, 2))),
#'   as.table(array(c(
#'               # prob of 1 then 2 given
#'     0.8, 0.2, # p1 = 1, p2 = 1
#'     0.2, 0.8, # p1 = 2, p2 = 1
#'     0.2, 0.8, # p1 = 1, p2 = 2
#'     0.2, 0.8  # p1 = 2, p2 = 2
#'   ), c(2, 2, 2)))
#' )
#' net <- bn(NULL, 1L, c(1L, 2L))
#' sim <- simulate(object = net, nsim = 1000, ptables = cpt)
simulate.bn <- function(object, nsim, seed, ptables, expectation = F, ...){
  stopifnot(
    "bn" %in% class(object),
    class(expectation) == "logical",
    length(expectation) == 1,
    length(nsim) == 1,
    class(nsim) %in% c("integer", "numeric"),
    class(ptables) == "list",
    all(sapply(ptables, class) == "table")
  )
  if (isTRUE(expectation)){
    creator <- expected.sample
  }
  else {
    creator <- sample.int
  }
  
  nNodes <- length(object)
  numberOfParents <- sapply(object, length)
  dat <- as.data.frame(matrix(NA, ncol = nNodes, nrow = nsim))
  
  # topologically sort
  nodeOrder <- topologicallyOrder(object)
  
  numberOfLevelsAll <- lapply(seq_len(nNodes), function(i){
    dim(ptables[[i]])[1]
  })
  
  # loop through the nodes in order
  for (i in nodeOrder){
    ptable <- ptables[[i]]
    
    # first dimension is the values
    numberOfLevels <- numberOfLevelsAll[[i]]
    
    if (numberOfParents[i] == 0){
      dat[, i] <- creator(numberOfLevels,
                             size    = nsim,
                             replace = T,
                             prob    = ptable)
    }
    else {
      parents <- object[[i]]
      
      local <- dat[, parents, drop = F]
      local <- data.frame(lapply(seq_len(ncol(local)), function(i){
        ls <- seq_len(numberOfLevelsAll[[parents[i]]])
        factor(local[, i], levels = ls)
      }))
      
      # find all the configurations of the parents that exist
      tab <- table(local)
      whichTab <- which(tab > 0, arr.ind = T)
      
      # loop over the configurations
      for (rowNum in seq.int(nrow(whichTab))){
        config <- whichTab[rowNum, ]
        
        # get conditional probability table for this configuraion
        L <- as.list(c(0, config))
        L[1] <- T
        probs <- do.call("[", c(list(ptable), L))
        
        # for the rows that are in this configuration, sample
        whichRows <- apply(local, 1, function(x) all(x == config))
        
        dat[whichRows, i] <- creator(numberOfLevels,
                                        size    = sum(whichRows),
                                        replace = T,
                                        prob    = probs)
      }
      
      # old slow version
      # would be better to loop over the distinct values, rather than rows
      #for (j in 1:nsim){
      #  config <- local[j, ]
      #  L <- as.list(c(0, config))
      #  L[1] <- T
      #  probs <- do.call("[", c(list(ptable), L))
      #  dat[j, i] <- sample.int(numberOfLevels,
      #                          size    = 1,
      #                          replace = T,
      #                          prob    = probs)
      #}
    }
  }
  
  data.frame(lapply(dat, as.factor))
}

#' ...
#' 
#' I don't think this works. In particular check that sample.int is 
#' sensible. And that N is properly implemented
#' @param bn ...
#' @param ptables ...
#' @param N ...
#' @export
marginal.probs <- function(bn, ptables, N){
  stopifnot(
    "bn" %in% class(bn),
    class(ptables) == "list",
    all(sapply(ptables, class) == "table")
  )
  
  nNodes <- length(bn)
  numberOfParents <- sapply(bn, length)
  
  # topologically sort
  nodeOrder <- topologicallyOrder(bn)
  
  numberOfLevelsAll <- lapply(seq_len(nNodes), function(i){
    dim(ptables[[i]])[1]
  })
  
  # loop through the nodes in order
  for (i in nodeOrder){
    ptable <- ptables[[i]]
    
    # first dimension is the values
    numberOfLevels <- numberOfLevelsAll[[i]]
    
    if (numberOfParents[i] == 0){
      dat[, i] <- sample.int(numberOfLevels,
                             size    = N,
                             replace = T,
                             prob    = ptable)
    }
    else {
      parents <- bn[[i]]
      
      local <- dat[, parents, drop = F]
      local <- data.frame(lapply(seq_len(ncol(local)), function(i){
        ls <- seq_len(numberOfLevelsAll[[parents[i]]])
        factor(local[, i], levels = ls)
      }))
      
      # find all the configurations of the parents that exist
      tab <- table(local)
      whichTab <- which(tab > 0, arr.ind = T)
      
      # loop over the configurations
      for (rowNum in seq.int(nrow(whichTab))){
        config <- whichTab[rowNum, ]
        
        # get conditional probability table for this configuraion
        L <- as.list(c(0, config))
        L[1] <- T
        probs <- do.call("[", c(list(ptable), L))
        
        # for the rows that are in this configuration, sample
        whichRows <- apply(local, 1, function(x) all(x == config))
        
        dat[whichRows, i] <- sample.int(numberOfLevels,
                                        size    = sum(whichRows),
                                        replace = T,
                                        prob    = probs)
      }
    }
  }
}

#' Create a binary Conditional Probability Table
#' 
#' @param i The probability of value 1
#' @param j The probability of value 2
#' @seealso \code{\link{simulate.bn}}
#' @export
#' @examples
#' cptBinary(0.2, 0.8)
cptBinary <- function(i, j){
  as.table(array(c(i, j), 2))
}
