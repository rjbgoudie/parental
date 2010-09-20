as.bn.pcAlgo <- function(x){
  require(pcalg)
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

as.parental.pcAlgo <- function(x){
  require(pcalg)
  stopifnot(
    "pcAlgo" %in% class(x)
  )
  as.parental(x@graph)
}

as.cpdag <- function(x, ...){
  UseMethod("as.cpdag")
}

as.cpdag.bn <- function(x){
  # Convert a parental BN to a 
  # Completed Partially Directed Acyclic Graph (CPDAG).
  #
  # Args:
  #   x: The parental BN to be converted.
  #
  # Returns:
  #   The CPDAG as a parental. (Which will in general not be a BN)
  require(pcalg)
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

as.cpdag2 <- function(x, ...){
  UseMethod("as.cpdag2")
}

whichVStructure <- function(net){
  nodeSeq <- seq_along(net)
  lapply(nodeSeq, function(i){
    if (i == 28) browser()
    parents <- net[[i]]
    if (length(parents) >= 2){
      parentsSeq <- seq_along(parents)
      grandparents <- lapply(parentsSeq, function(j){
        net[[parents[j]]]
      })
      allgrandparents <- unlist(grandparents)
      notInVStructure <- intersect(parents, allgrandparents)
      moreNotInVStructure <- unlist(lapply(parentsSeq, function(j){
        if (length(intersect(grandparents[[j]], notInVStructure)) > 0){
          parents[j]
        }
        else {
          integer(0)
        }
      }))
      out <- setdiff(parents, c(notInVStructure, moreNotInVStructure))
      if (is.null(out) || length(out) < 2){
        integer(0)
      } else {
        out
      }
    }
    else {
      integer(0)
    }
  })
}

whichVStructure <- function(net){
  nodeSeq <- seq_along(net)
  lapply(nodeSeq, function(i){
    parents <- net[[i]]
    if (length(parents) >= 2){
      parentsSeq <- seq_along(parents)
      grandparents <- lapply(parentsSeq, function(j){
        net[[parents[j]]]
      })
      allgrandparents <- unlist(grandparents)
      notInVStructure <- intersect(parents, allgrandparents)
      moreNotInVStructure <- unlist(lapply(parentsSeq, function(j){
        if (length(intersect(grandparents[[j]], notInVStructure)) > 0){
          parents[j]
        }
        else {
          integer(0)
        }
      }))
      out <- setdiff(parents, c(notInVStructure, moreNotInVStructure))
      if (is.null(out) || length(out) < 1){
        integer(0)
      } else {
        parents
      }
    }
    else {
      integer(0)
    }
  })
}

whichVStructurePlot <- function(net){
  vs <- whichVStructure(x)
  vs <- lapply(vs, function(parents){
    parents <- sort.int(parents)
    storage.mode(parents) <- "integer"
    parents
  })
  class(vs) <- "parental"
  adjvs <- as.adjacency(vs)
  grplot(net, edgecol = adjvs + 1)
}

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

as.cpdag2.bn <- function(bn){
  #### incorrect.
  #### need to make edges not in v-strcutures undirected
  bn <- makeNonVStructuresUndirected(bn)
  pdag <- as.adjacency(bn)
  out <- maximallyOrientEdges(pdag)
  out <- abs(out)
  as.parental(out)
}

#as.cpdag.bn <- as.cpdag2.bn

as.cpdag.bn.list <- function(x){
  # Convert a bn.list to list of parental.list of
  # Completed Partially Directed Acyclic Graph (CPDAG).
  #
  # Args:
  #   x: An object of class bn.list
  #
  # Returns:
  #   A parental.list containing a list of CPDAGs of class CPDAG.
  require(pcalg)
  stopifnot(class(x) == "bn.list")
  
  res <- lapply(x, as.cpdag)
  class(res) <- "parental.list"
  res
}

as.cpdag.bnpostmcmc.list <- function(x){
  
  # Convert a full set of MCMC posterior samples to 
  # Completed Partially Directed Acyclic Graph (CPDAG).
  # All MCMC runs are converted.
  #
  # Args:
  #   x: An object of class bnpostmcmc.list
  #
  # Returns:
  #   A list containing a list of CPDAGs of class CPDAG.
  require(pcalg)
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