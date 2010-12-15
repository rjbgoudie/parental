# returns a list of every DAG with n nodes

#' Enumerate the space of BNs
#' 
#' Enumerate (ie make a list of, not just count) the entire space of 
#' Bayesian Networks models, on a given number of nodes.
#' 
#' @param n The number of nodes
#' @param allowCyclic A logical indicating whether cyclic directed graphs
#'   should be filtered out.
#' @param multicore A logical specifying whether to use 
#'   \link[multicore]{mclapply}.
#' @return A \code{parental.list} including ALL the directed acyclic 
#'   graphs with \code{n} nodes.
#' @export
enumerateBNSpace <- function(n, allowCyclic = F, multicore = F){
  myapply <- function(...) lapply(...)
  if (multicore){
    require(multicore)
    myapply <- function(...) mclapply(..., mc.cores = 10)
  }
  
  combin <- function(element, n){
    unlist(
      lapply(seq_len(n-1), function(i){
        combn(
          setdiff(seq_len(n), element), # all elements except
                                        # the current element
          i, 
          simplify = F
        )
      }),
      recursive = F)
  }
  
  options <- lapply(seq_len(n), function(i) combin(i, n))
  
  # plus 1 for no parents
  choices <- expand.grid(lapply(options, function(option){
    seq_len(length(option) + 1)
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

#' Filter cyclic graphs
#' 
#' Removes the cyclic graphs from a list of graphs
#' 
#' @param bnlist A \code{parental.list} of graphs
#' @return An object of class \code{bn.list}, containing all the acyclic 
#'   graphs in \code{bnlist}
#' @export
filterCyclic <- function(bnlist){
  isAcyclic <- lapply(bnlist, checkAcyclic)
  out <- bnlist[which(isAcyclic == T)]
  class(out) <- c("bn.list", "parental.list")
  out
}

#' Create a bn.list
#' 
#' Creates a list of objects of \code{bn} class.
#' 
#' @param ... A number of objects of class \code{bn}
#' @return An object of class \code{bn.list}, containing all supplied BNs.
#' @export
bn.list <- function(...){
  out <- list(...)
  class(out) <- c("bn.list", "parental.list")
  out
}
