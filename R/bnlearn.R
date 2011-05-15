# Part of the "parental" package, http://github.com/rjbgoudie/parental
# 
# This software is distributed under the GPL-3 license.  It is free,
# open source, and has the attribution requirements (GPL Section 7) in
#   http://github.com/rjbgoudie/parental
# 
# Note that it is required that attributions are retained with each function.
#
# Copyright 2008 Robert J. B. Goudie, University of Warwick

#' Conversion from 'bnlearn' object
#' 
#' Convert a 'bnlearn' graph object to a 'parental' object.
#' 
#' This bridge to \code{bnlearn} can be temperamental because 
#' \code{bnlearn} also has an object called \code{bn}.
#' 
#' @param x A 'bnlearn' object, of class \code{bn} (the bnlearn version).
#' @return An object of class 'bn'.
#' bnlearn2parental()
#' @export
bnlearn2parental <- function(x){
  require(bnlearn)
  stopifnot("bn" %in% class(x),
            !is.null(x$arcs),
            !is.null(x$nodes))
  nNodes <- length(x$nodes)
  nodeNames <- names(x$nodes)
  nodeSeq <- seq_along(nodeNames)
  nEdges <- nrow(x$arcs)
  edgesSeq <- seq_len(nEdges)
  
  lookupTable <- list()
  lookupTable <- nodeSeq
  names(lookupTable) <- nodeNames
  
  edgelist <- matrix(ncol = 2, nrow = nEdges)
  for (row in edgesSeq){
    edgelist[row, 1] <- lookupTable[x$arcs[row, "from"]]
    edgelist[row, 2] <- lookupTable[x$arcs[row, "to"]]
  }
  out <- as.parental(edgelist, type = "edgelist", n = nNodes)
  names(out) <- nodeNames
  out
}

#' Conversion to 'bnlearn'
#' 
#' Convert a 'parental' object to a 'bnlearn' graph object
#' 
#' This bridge to \code{bnlearn} can be temperamental because 
#' \code{bnlearn} also has an object called \code{bn}.
#' 
#' @param x A \code{parental} graph
#' @return x A 'bnlearn' object, of class \code{bn} (the bnlearn version).
#' @export
parental2bnlearn <- function(x){
  require(bnlearn)
  stopifnot("parental" %in% class(x))
  
  nNodes <- nNodes(x)
  nodeSeq <- seq_len(nNodes)
  nodeNames <- if (!is.null(names(x))){
    names(x)
  } else {
    as.character(nodeSeq)
  }
  
  out <- empty.graph(nodeNames, num = 1)
  
  for (j in nodeSeq){
    for (i in x[[j]]){
      if (j %in% x[[i]]){
        out <- add.undirected.edge(x            = out,
                                   from         = nodeNames[i],
                                   to           = nodeNames[j],
                                   check.cycles = F)
      }
      else {
        out <- set.arc(x            = out,
                       from         = nodeNames[i],
                       to           = nodeNames[j],
                       check.cycles = F)
      }
    }
  }
  out
}


#' Add undirected edge to a 'bnlearn' object
#' 
#' Adding undirected edges is not an exported function in \code{bnlearn}, so 
#' this function exposes this functionality.
#' 
#' This bridge to \code{bnlearn} can be temperamental because 
#' \code{bnlearn} also has an object called \code{bn}.
#' 
#' @param x A 'bnlearn' object, of class \code{bn}.
#' @param from A character string, the label of a node.
#' @param to A character string, the label of a node.
#' @param check.cycles a boolean value. If TRUE the graph is tested for 
#'   acyclicity; otherwise the graph is returned anyway.
#' @param update A logical
#' @param debug a boolean value. If TRUE a lot of debugging output is 
#'   printed; otherwise the function is completely silent.
#' @return A new \code{bnlearn} object
#' @export
add.undirected.edge <- function(x, from, to, check.cycles,
                                update = T, debug = F) {
  stop("'op' required")
  op <- 1
  # check x's class.
  bnlearn::check.bn(x)
  # a valid node is needed.
  bnlearn::check.nodes(nodes = from, graph = x, max.nodes = 1)
  # another valid node is needed.
  bnlearn::check.nodes(nodes = to, graph = x, max.nodes = 1)
  # 'from' must be different from 'to'.
  if (identical(from, to))
    stop("'from' and 'to' must be different from each other.")
  # check logical flags (debug, check.cycles, update).
  bnlearn::check.logical(debug)
  bnlearn::check.logical(check.cycles)
  bnlearn::check.logical(update)
  
  # add undirected edge
  if (debug) cat("* setting arc", from, "->", to, ".\n")
  x$arcs = add.undirected.backend(from, to, x$arcs, debug = debug)


  # check whether the graph is still acyclic; 
  # not needed if an arc is dropped.
  if (check.cycles && (op != "drop"))
    if (!bnlearn::is.acyclic(x$arcs, names(x$nodes)))
      stop("the resulting graph contains cycles.")

  # update the network structure.
  if (update) {
    # build the adjacency matrix only once.
    amat = bnlearn::arcs2amat(x$arcs, names(x$nodes))
    # check which nodes have to be updated.
    updated.nodes = unique(c(from, to, x$nodes[[from]]$mb, x$nodes[[to]]$mb))
    # update the chosen nodes.
    for (node in updated.nodes)
      x$nodes[[node]] = bnlearn::cache.partial.structure(names(x$nodes),
        target = node, amat = amat, debug = debug)
  }#THEN
  invisible(x)
}#ARC.OPERATIONS

#' (Internal) Add undirected edge to a 'bnlearn' object
#' 
#' This bridge to \code{bnlearn} can be temperamental because 
#' \code{bnlearn} also has an object called \code{bn}.
#' 
#' @param from A character string, the label of a node.
#' @param to A character string, the label of a node.
#' @param arcs A matrix of two columns of character strings
#' @param debug a boolean value. If TRUE a lot of debugging output is 
#'   printed; otherwise the function is completely silent.
add.undirected.backend <- function(from, to, arcs, debug = F) {
  # the arc is there, undirected
  if (bnlearn::is.listed(arcs, c(to, from), both = T)){
    # nothing to do
    arcs
  }
  # the arc is there, but reversed.
  else if (bnlearn::is.listed(arcs, c(to, from))) {
    if (debug){
      cat("  > the arc", to, "->", from, "is present, reversing.\n")
    }
    rbind(arcs, c(from, to))
  }#THEN
  # the arc is already there.
  else if (bnlearn::is.listed(arcs, c(from, to))) {
    if (debug){
      cat("  > the arc", from, "->", to, "is present, reversing.\n")
    }
    rbind(arcs, c(to, from))

  }#THEN
  # the arc is not present.
  else {
    rbind(arcs, c(to, from), c(from, to))
  }
  
}#REVERSE.ARC.BACKEND
