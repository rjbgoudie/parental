# Part of the "parental" package, http://github.com/rbtgde/parental
# 
# This software is distributed under the GPL-3 license.  It is free,
# open source, and has the attribution requirements (GPL Section 7) in
#   http://github.com/rbtgde/parental
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
#' @param multicore A logical specifying whether to use 
#'   \link[multicore]{mclapply}.
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
                             multicore = F){
  myapply <- function(...) lapply(...)
  if (multicore){
    require(multicore)
    myapply <- function(...) mclapply(..., mc.cores = 10)
  }
  stopifnot(inherits(n, c("numeric", "integer")),
            is.logical(allowCyclic),
            length(allowCyclic) == 1,
            length(banned)      == n,
            length(required)    == n,
            is.logical(multicore),
            length(multicore)   == 1)
  required <- lapply(required, function(x){
    storage.mode(x) <- "integer"
    x
  })
  banned <- lapply(banned, function(x){
    storage.mode(x) <- "integer"
    x
  })
  
  combin <- function(element, n, banned, required){
    nRequired <- length(required)
    nBanned <- length(banned)
    s <- seq_len(n - 1 - nBanned - nRequired)
    if (length(s) == 0){
      out <- list(list(required))
    } else {
      out <- lapply(c(0, s), function(i){
        banned <- c(banned, element, required)
        x <- setdiff(seq_len(n), banned) # all elements except banned/required
                                          # elements and the current element
        combn3(x, i, required = required)
      })
    }
    unlist(out, recursive = F)
  }
  
  options <- lapply(seq_len(n), function(i){
    combin(i, n, banned[[i]], required[[i]])
  })

  # plus 1 for no parents
  choices <- expand.grid(lapply(options, function(option){
    seq_len(length(option))
  }))
  
  family <- myapply(seq_len(nrow(choices)), function(i){
    out <- unlist(
      lapply(seq_len(n), function(node){
        out <- options[[node]][choices[i,node]]
        if (is.null(out[[1]])){
          out[[1]] <- integer(0)
        }
        out
      }), 
      rec = F
    )
    class(out) <- c("bn", "parental")
    out
  })
  
  class(family) <- c("bn.list", "parental.list")
  
  if (allowCyclic){
    warning("** NOTE: ALLOWING CYCLIC GRAPHS **")
  }
  else {
    family <- filterCyclic(family)
  }
  
  family
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
