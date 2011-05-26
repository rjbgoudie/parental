# Part of the "parental" package, http://github.com/rjbgoudie/parental
# 
# This software is distributed under the GPL-3 license.  It is free,
# open source, and has the attribution requirements (GPL Section 7) in
#   http://github.com/rjbgoudie/parental
# 
# Note that it is required that attributions are retained with each function.
#
# Copyright 2008 Robert J. B. Goudie, University of Warwick

# returns a list of every DAG with n nodes

#' Enumerate the space of BNs.
#' 
#' Enumerate (ie make a list of, not just count) the entire space of 
#' Bayesian Networks models, on a given number of nodes.
#' 
#' @param n The number of nodes
#' @param allowCyclic A logical indicating whether cyclic directed graphs
#'   should be filtered out.
#' @param banned A list of length \code{n}. Each component indicates which 
#'   nodes cannot be parents of the corresponding node.
#' @param required A list of length \code{n}. Each component indicates
#'   which nodes must be includes as parents of the corresponding node.
#' @param maxIndegree The maximum indegree of all nodes. A numeric of length
#'   1.
#' @param multicore A logical specifying whether to use 
#'   \link[multicore]{mclapply}.
#' @param check Should the input be checked for validity?
#' @return A \code{parental.list} including ALL the directed acyclic 
#'   graphs with \code{n} nodes.
#' @export
#' @examples
#' enumerateBNSpace(3)
#' enumerateBNSpace(3, banned = list(c(), 1, c()))
#' 
#' x <- enumerateBNSpace(3, allowCyclic = TRUE)
#' filterCyclic(x)
enumerateBNSpace <- function(n,
                             allowCyclic = F,
                             banned = vector("list", n),
                             required = vector("list", n),
                             maxIndegree = n - 1,
                             multicore = F,
                             check = T){
  myapply <- function(...) lapply(...)
  if (multicore){
    require(multicore)
    myapply <- function(...) mclapply(..., mc.cores = 10)
  }
  if (check){
    stopifnot(inherits(n, c("numeric", "integer")),
              is.logical(allowCyclic),
              length(allowCyclic) == 1,
              length(banned)      == n,
              length(required)    == n,
              length(maxIndegree) == 1,
              inherits(maxIndegree, c("numeric", "integer")),
              is.wholenumber(maxIndegree),
              all(findInterval(maxIndegree, c(0, n - 1), right = T) == 1),
              max(sapply(required, length)) <= maxIndegree,
              is.logical(multicore),
              length(multicore)   == 1)
  }
  required <- lapply(required, function(x){
    storage.mode(x) <- "integer"
    x
  })
  banned <- lapply(banned, function(x){
    storage.mode(x) <- "integer"
    x
  })
  
  combin <- function(element, n, banned, required, maxIndegree){
    nRequired <- length(required)
    nBanned <- length(banned)
    numChoosable <- min(n - 1 - nBanned, maxIndegree)
    numLeft <- max(numChoosable - nRequired, 0)
    if (numLeft == 0){
      out <- list(list(required))
    } else {
      out <- vector("list", length = numLeft + 1)
      i = 0
      while (i <= numLeft){
        banned <- c(banned, element, required)
        x <- setdiff(seq_len(n), banned) # all elements except banned/required
                                          # elements and the current element
        out[[i + 1]] <- combn3(x, i, required = required)
        i <- i + 1
      }
    }
    unlist(out, recursive = F)
  }

  options <- vector("list", length = n)
  choicesOptions <- vector("list", length = n)
  i <- 1
  while (i <= n){
    options[[i]] <- combin(i, n, banned[[i]], required[[i]], maxIndegree)
    choicesOptions[[i]] <- seq_len(length(options[[i]]))
    i <- i + 1
  }

  choices <- data.matrix(expand.grid(choicesOptions))
  nChoices <- nrow(choices)
  family <- vector("list", length = nChoices)
  bnempty <- empty(n, "bn")
  routesempty <- matrix(0, n, n)
  diag(routesempty) <- 1
  i <- 1
  wh <- 1
  
  while (i <= nChoices){
    bn <- bnempty
    r <- routesempty
    node <- 1
    cycle <- F
    while (node <= n & !cycle){
      new <- options[[node]][[choices[i, node]]]
      if (any(r[node, new] != 0)){
        cycle <- T
      } else {
        if (is.null(new)){
          new <- integer(0)
        }
        bn[[node]] <- new
        r <- routesAddEdges(r, new, node)
        node <- node + 1
      }
    }
    if (!cycle){
      family[[wh]] <- bn
      wh <- wh + 1
    }
    i <- i + 1
  }
  family <- family[seq_len(wh - 1)]
  
  class(family) <- c("bn.list", "parental.list")

  family
}

routesAddEdges <- function(x, i, j){
  if (length(i) > 0){
    x + rowSumsFast(x[, i, drop = F]) * x[rep(j, dim(x)[1]), ]
  } else {
    x
  }
}

rowSumsFast <- function(x){
  dn <- dim(x)
  .Internal(rowSums(x, prod(dn[1L]), prod(dn[-1L]), FALSE))
}

#' Filter cyclic graphs.
#' 
#' Removes the cyclic graphs from a list of graphs
#' 
#' @param bnlist A \code{parental.list} of graphs
#' @return An object of class \code{bn.list}, containing all the acyclic 
#'   graphs in \code{bnlist}
#' @export
#' @seealso \code{\link{enumerateBNSpace}}
#' @examples
#' x <- enumerateBNSpace(3, allowCyclic = TRUE)
#' filterCyclic(x)
filterCyclic <- function(bnlist){
  isAcyclic <- lapply(bnlist, checkAcyclic)
  out <- bnlist[which(isAcyclic == T)]
  class(out) <- c("bn.list", "parental.list")
  out
}

