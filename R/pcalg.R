# Part of the "parental" package, http://github.com/rjbgoudie/parental
# 
# This software is distributed under the GPL-3 license.  It is free,
# open source, and has the attribution requirements (GPL Section 7) in
#   http://github.com/rjbgoudie/parental
# 
# Note that it is required that attributions are retained with each function.
#
# Copyright 2008 Robert J. B. Goudie, University of Warwick

#' Coerce pcAlgo object to a bn.
#'
#' ....
#' 
#' @param x An object of class \code{pcAlgo}
#' @param ... unused
#' @S3method as.bn pcAlgo
#' @method as.bn pcAlgo
as.bn.pcAlgo <- function(x, ...){
  if (require(pcalg)){
    stopifnot(
      "pcAlgo" %in% class(x)
    )
    warning("this selects a particular bn from the equivalence class")
    pdag <- pdag2dag(x@graph)
    if (isTRUE(pdag$success)){
      as.bn(pdag$graph)
    }
    else {
      stop("Extension via pdag2dag not possible")
    }
  }
}

#' Coerce pcAlgo object to a bn.
#'
#' ....
#' 
#' @param x An object of class \code{pcAlgo}
#' @param ... Further arguments (unused)
#' @S3method as.parental pcAlgo
#' @method as.parental pcAlgo
as.parental.pcAlgo <- function(x, ...){
  if (require(pcalg)){
    stopifnot(
      "pcAlgo" %in% class(x)
    )
    as.parental(x@graph)
  }
}

#' Convert an object to a CPDAG.
#' 
#' A generic
#' 
#' @param x An object
#' @param ... Further arguments passed to method
#' 
#' @export
as.cpdag <- function(x, ...){
  UseMethod("as.cpdag")
}

#' Find V Structure.
#' 
#' @param net ...
#' @export
whichVStructure <- function(net){
  nodeSeq <- seq_along(net)
  lapply(nodeSeq, function(node){
    parents <- net[[node]]
    if (length(parents) >= 2){
      parentsSeq <- seq_along(parents)
      grandparents <- lapply(parentsSeq, function(j){
        net[[parents[j]]]
      })
      
      # an edge j -> node participates in a v-structure if and only if
      # there exists a k in parent(node)\{j} s.t 
      # 1. there is no edge j -> k
      # 2. there is no edge k -> j
      
      parents[unlist(lapply(parentsSeq, function(j){
        # check whether all possible k are linked to j
        
        # is there an edge j -> k
        condition1 <- parents[-j] %in% grandparents[[j]]
        # is there an edge k -> j
        condition2 <- sapply(grandparents[-j], function(k){
          parents[j] %in% k
        })
        
        # do all k have links to or from j?
        if (all(condition1 | condition2)){
          # if so, j is not in a v-structure
          integer(0)
        }
        else {
          # otherwise it is
          j
        }
      }), use.names = F)]

    }
    else {
      integer(0)
    }
  })
}

#' Undocumented.
#'
#' ...
#'
#' @param net ...
#' @return ...
#' @export
whichVStructurePlot <- function(net){
  vs <- whichVStructure(net)
  vs <- lapply(vs, function(parents){
    parents <- sort.int(parents)
    storage.mode(parents) <- "integer"
    parents
  })
  class(vs) <- "parental"
  adjvs <- as.adjacency(vs)
  grplot(net, edgecol = adjvs + 1)
}

#' Undocumented.
#'
#' ...
#'
#' @param bn ...
#' @return ...
#' @export
makeNonVStructuresUndirected <- function(bn){
  parentsSeq <- seq_along(bn)
  ivs <- whichVStructure(bn)
  bn2 <- bn
  for (i in parentsSeq){
    toMakeUndirected <- setdiff(bn[[i]], ivs[[i]])
    for (j in toMakeUndirected){
      bn2[[j]] <- sort.int(c(bn2[[j]], i))
      storage.mode(bn2[[j]]) <- "integer"
    }
  }
  class(bn2) <- "parental"
  bn2
}

#' Convert a bn to a CPDAG2 (?).
#'
#' Not sure this is correct
#'
#' @param x An object
#' @param ... Further arguments (unused)
#' @return ...
#' @S3method as.cpdag bn
#' @method as.cpdag bn
as.cpdag.bn <- function(x, ...){
  #### incorrect.
  #### need to make edges not in v-strcutures undirected
  x <- makeNonVStructuresUndirected(x)
  pdag <- as.adjacency(x)
  out <- maximallyOrientEdges(pdag)
  
  out <- abs(out)
  as.parental(out)
}

#' Convert 'bn list' to CPDAGs.
#' 
#' Convert a bn.list to list of parental.list of
#' Completed Partially Directed Acyclic Graph (CPDAG).
#'
#' @param x An object of class bn.list
#' @param verbose Should a progress bar be shown?
#' @param ... Further arguments (unused)
#' @return A parental.list containing a list of CPDAGs of class CPDAG.
#' @S3method as.cpdag bn.list
#' @method as.cpdag bn.list
as.cpdag.bn.list <- function(x, verbose = T, ...){
  stopifnot("bn.list" %in% class(x))
  
  len <- length(x)
  res <- vector("list", length = len)
  if (verbose){
    progress <- txtProgressBar(max = len, style = 3)
    setTxtProgressBar(progress, 0)
  }
  i <- 1
  while (i <= len){
    if (verbose){
      setTxtProgressBar(progress, i)
    }
    res[[i]] <- as.cpdag(x[[i]])
    i <- i + 1
  }
  close(progress)
  class(res) <- "parental.list"
  res
}

#' Convert 'bnpostmcmc list' to CPDAGs.
#' 
#' Convert a full set of MCMC posterior samples to 
#' Completed Partially Directed Acyclic Graph (CPDAG).
#' All MCMC runs are converted.
#'
#' @param x An object of class bnpostmcmc.list
#' @param ... Further arguments (unused)
#' @return A list containing a list of CPDAGs of class CPDAG.
#' @S3method as.cpdag bnpostmcmc.list
#' @method as.cpdag bnpostmcmc.list
as.cpdag.bnpostmcmc.list <- function(x, ...){
  stopifnot(
    class(x)        ==   "bnpostmcmc.list",
    "parental.list" %in% class(x[[1]]$samples),
    "parental"      %in% class(x[[1]]$samples[[1]]),
    length(x)       >=   1)
  
  runSeq <- seq_along(x)
  lapply(runSeq, function(whichRun){
    res <- lapply(x[[whichRun]]$samples, as.cpdag)
    class(res) <- "parental.list"
    res
  })
}

#' Convert an object to a CPDAG.
#' 
#' A generic function
#' 
#' @param x An object
#' @param ... Further arguments passed to methods
#' 
#' @export
as.cpdag0 <- function(x, ...){
  UseMethod("as.cpdag0")
}

#' Convert 'bn' to CPDAG.
#' 
#' Convert a parental BN to a 
#' Completed Partially Directed Acyclic Graph (CPDAG).
#'
#' @param x The parental BN to be converted.
#' @param ... Further arguments (unused)
#' @return The CPDAG as a parental. (Which will in general not be a BN)
#' @S3method as.cpdag0 bn
#' @method as.cpdag0 bn
as.cpdag0.bn <- function(x, ...){
  if (require(pcalg)){
    stopifnot(
      "bn" %in% class(x)
    )
  
    #gr <- as.graph(x)
    # The heavy lifting is performed by dag2cpdag() in package pcalg
    #cpdag <- dag2cpdag(gr)
    p <- length(x)
    if (nEdges(x) == 0){
      class(x) <- "parental"
      x
    } else {
      dag <- as.adjacency(x)
      # The following code is extracted from dag2cpdag() in package pcalg
      # to save needless type conversions
      # by Markus Kalisch
      dag[dag != 0] <- 1

      ## dag is adjacency matrix
      e.df <- labelEdges(dag)
      cpdag <- matrix(rep(0, p * p), nrow = p, ncol = p)
      for (i in seq_len(dim(e.df)[1])) {
        if (e.df$label[i]) {
          cpdag[e.df$tail[i], e.df$head[i]] <- 1
        } else {
          cpdag[e.df$tail[i],e.df$head[i]] <- 1
          cpdag[e.df$head[i],e.df$tail[i]] <- 1
        }
      }
      as.parental(cpdag)
    }
  }
}

#' Convert 'bn list' to CPDAG (old version).
#' 
#' Convert a bn.list to list of parental.list of
#' Completed Partially Directed Acyclic Graph (CPDAG).
#'
#' @param x An object of class bn.list
#' @param ... Further arguments (unused)
#' @return A parental.list containing a list of CPDAGs of class CPDAG.
#' @S3method as.cpdag0 bn.list
#' @method as.cpdag0 bn.list
as.cpdag0.bn.list <- function(x, ...){
  stopifnot(class(x) == "bn.list")
  
  res <- lapply(x, as.cpdag0)
  class(res) <- "parental.list"
  res
}

#' Convert 'bnpostmcmc list' to CPDAG (old version).
#' 
#' Convert a full set of MCMC posterior samples to 
#' Completed Partially Directed Acyclic Graph (CPDAG).
#' All MCMC runs are converted.
#'
#' @param x An object of class bnpostmcmc.list
#' @param ... Further arguments (unused)
#' @return A list containing a list of CPDAGs of class CPDAG.
#' @S3method as.cpdag0 bnpostmcmc.list
#' @method as.cpdag0 bnpostmcmc.list
as.cpdag0.bnpostmcmc.list <- function(x, ...){
  stopifnot(
    class(x)        ==   "bnpostmcmc.list",
    "parental.list" %in% class(x[[1]]$samples),
    "parental"      %in% class(x[[1]]$samples[[1]]),
    length(x)       >=   1)
  
  runSeq <- seq_along(x)
  lapply(runSeq, function(whichRun){
    res <- lapply(x[[whichRun]]$samples, as.cpdag0)
    class(res) <- "parental.list"
    res
  })
}
