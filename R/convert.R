# Part of the "parental" package, http://github.com/rjbgoudie/parental
# 
# This software is distributed under the GPL-3 license.  It is free,
# open source, and has the attribution requirements (GPL Section 7) in
#   http://github.com/rjbgoudie/parental
# 
# Note that it is required that attributions are retained with each function.
#
# Copyright 2008 Robert J. B. Goudie, University of Warwick

#' Convert an object to an adjacency matrix.
#' 
#' A generic for converting objects to adjacency matrices.
#' 
#' An adjacency matrix is a matrix A of dimension n x n, with A[i, j] = 1 if 
#' an edge exist between nodes i and j, where n is the number of nodes.
#' 
#' @param x An object
#' @param ... Further arguments, passed to method
#' @seealso \code{\link{as.adjacency.parental}}
#' @export
#' @examples
#' x <- parental(c(), c(1), c(2))
#' as.adjacency(x)
as.adjacency <- function(x, ...) {
  UseMethod("as.adjacency")
}

#' Convert an object to a bn.
#' 
#' A generic function that converts objects to \code{bn} objects
#' 
#' @param x An object
#' @param ... Further arguments, passed to method
#' 
#' @export
#' @seealso \code{\link{as.bn.matrix}}, \code{\link{as.bn.character}}, 
#'   \code{\link{as.bn.graphNEL}}
#' @examples
#' edgelist <- matrix(c(c(1,2), c(2, 3)), byrow = TRUE, nrow = 2, ncol = 2)
#' colnames(edgelist) <- c("row", "col")
#' as.bn(edgelist, type = "edgelist", n = 3)
#' 
#' x <- "[][1][1,2][3]"
#' as.bn(x, pretty = TRUE)
#' 
#' if (require(graph)){
#'   set.seed(123)
#'   c1 <- c(1, 1, 2, 4)
#'   names(c1) <- as.character(1:4)
#'   g1 <- randomNodeGraph(c1)
#'   as.parental(g1)
#' }
as.bn <- function(x, ...) {
  UseMethod("as.bn")
}

#' Convert an object to a graph.
#' 
#' graphNEL objects are from bioconductor package 'graph'. See 
#' \link[graph]{graphNEL-class}.
#' 
#' @param x An object
#' @param ... Further arguments, passed to method
#' @seealso \code{\link{as.graph.parental}}
#' @export
#' @examples
#' if (require(graph)){
#'   x <- bn(integer(0), 1, 2)
#'   y <- as.graph(x)
#'   edges(y)
#' }
as.graph <- function(x, ...){
  UseMethod("as.graph")
}

#' Convert a matrix to a 'bn'.
#' 
#' See for 'as.parental.matrix' for details
#' 
#' @param x A matrix, as described in as.parental.matrix()
#' @param ... Passed to as.parental.matrix
#' @return An object of class 'bn'
#' @S3method as.bn matrix
#' @method as.bn matrix
#' @examples
#' edgelist <- matrix(c(c(1,2), c(2, 3)), byrow = TRUE, nrow = 2, ncol = 2)
#' colnames(edgelist) <- c("row", "col")
#' as.bn(edgelist, type = "edgelist", n = 3)
as.bn.matrix <- function(x, ...){
  class(x) <- "matrix"
  out <- as.parental(x, ...)
  if (checkAcyclic(out)){
    class(out) <- c("bn", "parental")
    out
  }
  else {
    stop("Not a DAG")
  }
}

#' Convert a object of class 'parental' to an adjacency matrix.
#' 
#' An adjacency matrix is a matrix A of dimension n x n, with A[i, j] = 1 if 
#' an edge exist between nodes i and j, where n is the number of nodes.
#' 
#' @param x A object of class 'parental'.
#' @param ... unused
#' @return An adjacency matrix.
#' @S3method as.adjacency parental
#' @method as.adjacency parental
#' @examples
#' x <- parental(c(), c(1), c(2))
#' as.adjacency(x)
as.adjacency.parental <- function(x, ...){
  stopifnot("parental" %in% class(x))
  n <- nNodes(x)
  m <- matrix(0, n, n)
  
  # for (i in 1:n){
  #     for (j in 1:n){
  #       m[i, j] <- if(i %in% x[[j]]){
  #         1
  #       }
  #       else {
  #         0
  #       }
  #     }
  #   }
  for (j in seq.int(n)){
    m[x[[j]], j] <- 1
  }

  m
}

#' Convert an object to a parental object.
#' 
#' A generic function for converting objects to \code{\link{parental}} 
#' objects.
#' 
#' @param x An object
#' @param ... Further arguments passed to method
#' @seealso \code{\link{as.parental.bn}}, \code{\link{as.parental.matrix}},
#'   \code{\link{as.parental.character}}, \code{\link{as.parental.graphNEL}}.
#' @export
#' @examples
#' x <- bn(c(), c(1), c(1, 2))
#' as.parental(x)
#' 
#' adjmat <- matrix(sample(c(0, 1), 100, rep = TRUE), 10, 10)
#' diag(adjmat) <- 0
#' as.parental(adjmat)
#' 
#' edgelist <- matrix(c(1, 2), nrow = 1, ncol = 2)
#' colnames(edgelist) <- c("row", "col")
#' as.parental(edgelist, type = "edgelist", n = 2)
#' 
#' x <- "[3][1][2][1][1,3]"
#' as.parental(x, pretty = TRUE)
#' 
#' x <- "3,1,2,1,c(1,3)"
#' as.parental(x)
#' 
#' if (require(graph)){
#'   set.seed(123)
#'   c1 <- c(1, 1, 2, 4)
#'   names(c1) <- as.character(1:4)
#'   g1 <- randomNodeGraph(c1)
#'   as.parental(g1)
#' }
as.parental <- function(x, ...){
  UseMethod("as.parental")
}

#' Convert a character string to a parental.
#' 
#' Format required is one of two. Either standard or pretty. See examples.
#'
#' @param x An character object
#' @param pretty A logical indicating whether the supplied object is pretty
#' @param ... Further arguments (unused)
#' @return A \code{parental} object.
#' @S3method as.parental character
#' @method as.parental character
#' @examples
#' x <- "[3][1][2][1][1,3]"
#' as.parental(x, pretty = TRUE)
#' 
#' x <- "3,1,2,1,c(1,3)"
#' as.parental(x)
as.parental.character <- function(x, pretty = F, ...){
  if (pretty){
    # handle nodes with no parents
    x <- gsub("\\[\\]", "\\[integer(0)\\]", x)
    
    # trim off leading [
    # and trailing ]
    x <- substr(x, 2, nchar(x) - 1)
    
    # split into nodes
    xsplit <- strsplit(x, "][", fixed = T)[[1]]
    
    # wrap up into the correct format
    x <- paste(paste("c(", xsplit, ")", sep = ""), collapse = ",")
  }
  else {
    if (any(seemsPretty(x))){
      warning("It seems you submitted a pretty character. Try pretty = T")
    }
  }
  
  out <- lapply(x, function(id){
    temp <- eval(parse(text = paste("list(", id, ")", sep = "")))
    temp <- lapply(temp, as.integer)
    class(temp) <- "parental"
    temp
  })
  if (length(x) == 1){
    out <- unlist(out, recursive = F)
    class(out) <- "parental"
  }
  else {
    class(out) <- "parental.list"
  }
  out
}

#' Convert a character vector to a 'bn'.
#' 
#' Format required is one of two. Either standard or pretty. See examples.
#'
#' @param x A character object
#' @param checkAcyclic Should the bn be checked for cycles? A logical.
#' @param ... Further arguments (unused)
#' @return A object of class \code{bn}.
#' @S3method as.bn character
#' @method as.bn character
#' @examples
#' x <- "[][1][1,2][3]"
#' as.bn(x, pretty = TRUE)
as.bn.character <- function(x, checkAcyclic = T, ...){
  out <- as.parental(x, ...)
  if ("parental.list" %in% class(out)){
    if (!isTRUE(checkAcyclic)){
      warning("Not checking for cycles!")
    }
    out <- lapply(out, function(bn){
      if (isTRUE(checkAcyclic)){
        if (!isTRUE(checkAcyclic(bn))){
          stop("Contains a cyclic graph")
        }
      }
      class(bn) <- c("bn", "parental")
      bn
    })
    class(out) <- c("bn.list", "parental.list")
    out
  }
  else {
    if (isTRUE(checkAcyclic)){
      if (!isTRUE(checkAcyclic(out))){
        stop("Contains a cyclic graph")
      }
    }
    else {
      warning("Not checking for cycles!")
    }
    class(out) <- c("bn", "parental")
    out
  }
}

#' Convert a variety of matrix objects to 'parental' objects.
#' 
#' The supplied matrix x should either be an adjacency 
#' matrix (type = "adjacency"), or an edgelist (type = "edgelist").
#' Adjacency matrices should be an n x n matrix A with A[i, j] = 1 if an 
#' edge exists between node i and j. Edgelists are of the form returned 
#' by which(x == condition, arr.ind = TRUE) for matrices x in 
#' adjacency matrix form.
#' 
#' For type = "edgelist", the number of nodes n must be specified.
#' 
#' @param x An object of class 'matrix'
#' @param type Either "adjacency" or "edgelist"
#' @param n The number of nodes n. Only required for type = "edgelist"
#' @param ... Further arguments (unused)
#' @return An object of class 'parental'.
#' @S3method as.parental matrix
#' @method as.parental matrix
#' @examples
#' adjmat <- matrix(sample(c(0, 1), 100, rep = TRUE), 10, 10)
#' diag(adjmat) <- 0
#' as.parental(adjmat)
#' 
#' edgelist <- matrix(c(1, 2), nrow = 1, ncol = 2)
#' colnames(edgelist) <- c("row", "col")
#' as.parental(edgelist, type = "edgelist", n = 2)
as.parental.matrix <- function(x, type = "adjacency", n, ...){
  if (type == "adjacency"){
    if (class(x) != "matrix"){
      stop("Adjacency must be a matrix")
    }
    if (length(dim(x)) != 2){
      stop("Must be two dimensional")
    }
    if (dim(x)[1] != dim(x)[2]){
      stop("Must be square matrix")
    }
  
    nNodes <- dim(x)[1]
    out <- lapply(seq_len(nNodes), function(i){
      which(x[, i] == 1) # returns integer
    })
  }
  else if (type == "edgelist"){
    stopifnot(class(x) == "matrix",
              isTRUE(missing(n)) == F)
    if (class(x) != "matrix"){
      stop("need a matrix for now")
    }
  
    if (any(colnames(x) != c("row", "col"))){
      stop("colnames must be row and col")
    }
    
    # this is at least consistent with edgeprobs!
    colnames(x) <- c("tail", "head")
  
    out <- lapply(seq_len(n), function(i) integer(0))
  
    if (nrow(x) > 0){
      apply(x, 1, function(row){
        head <- unname(row["head"])
        tail <- unname(row["tail"])
        out[[head]] <<- c(out[[head]], as.integer(tail))
      })
    }
  }
  class(out) <- "parental"
  out
}

#' Convert a 'bn' to a 'parental'.
#' 
#' @param x An object of class 'bn'
#' @param ... Further arguments (unused)
#' @return The object 'x', but of class 'parental'
#' @S3method as.parental bn
#' @method as.parental bn
#' @examples
#' x <- bn(c(), c(1), c(1, 2))
#' as.parental(x)
as.parental.bn <- function(x, ...){
  class(x) <- "parental"
  x
}

# Note:
# Pretty printing of integer sequences c(1L,2L,3L)
# goes to
# 1:3
#' Convert a parental object to a character vector.
#' 
#' Converts a parental object to a character representation.
#'
#' @param x A character vector
#' @param pretty A logical, which determines the format of the output
#' @param ... Further arguments (unused)
#' @return An object of class "parental"
#' @S3method as.character parental
#' @method as.character parental
#' @examples
#' x <- parental(c(3), c(1), c(1, 2))
#' as.character(x)
#' 
#' x <- parental(c(3), c(1), c(1, 2))
#' as.character(x, pretty = TRUE)
as.character.parental <- function (x, pretty = F, ...) {
  if (pretty){
    # Collapse to comma separated, and paste together
    # then unlist
    # then 
    out <- paste(unlist(lapply(x, paste, collapse = ",", sep = ""), 
                    use.names = F, recursive = F), sep = "", collapse= "][")
    
    # the big problem with this approach is the as.character
    # limit for deparsing language objects at 500L
    
    #res <- paste(deparse(x), collapse = "")
    #browser()
    #res <- gsub(" ", "", substr(res, 16, nchar(res) - 1))
    #res <- replaceCommasWithinVectors(res)
    #paste("[",
    #  gsub(";", ",", # switch semicolons to comma
    #  gsub(",", "][", # remove commas
    #  gsub("c", "", # remove c
    #  gsub("integer0", "", # remove integer0
    #  gsub(")", "", # remove closing brackets
    #  gsub("(", "", # remove opening brackets
    #  gsub(" ", "", res # remove spaces
    #, fixed = T), fixed = T), fixed = T), fixed = T), fixed = T), 
    #        fixed = T), fixed = T), 
    #"]", sep = "") # add closing ]
    #
    out <- gsub(" ", "", out, fixed = T) # remove spaces
    paste("[", out, "]", collapse = "", sep = "")
  }
  else {
    out <- .Internal(paste(list(unclass(x)), "", ","))
    gsub(" ", "", out, fixed = T) # remove spaces
    #res <- 
    #res <- paste(deparse(x), collapse = "")
    #gsub("L", "", gsub(" ", "", substr(res, 16, nchar(res) - 31), 
    # fixed = T), fixed = T)
  }
}

#' Convert a 'parental list' to a character vector.
#' 
#' @param x A object of class 'parental.list'. A list of \code{parental} 
#'   objects
#' @param pretty A logical of length 1 indicating if the character vectors 
#'   are pretty-printed.
#' @param ... Further arguments (unused)
#' @return A character vector.
#' @S3method as.character parental.list
#' @method as.character parental.list
#' @examples
#' p1 <- parental(c(), c(1), c(1, 2, 4), c(3))
#' p2 <- parental(c(2), c(), c(4), c(2, 3))
#' x <- parental.list(p1, p2)
#' as.character(x)
as.character.parental.list <- function (x, pretty = F, ...) {
  sapply(x, as.character, pretty)
  #if (pretty == F){
  #  #res <- as.character(unclass(x))
  #  #gsub(" ", "", substr(res, 6, sapply(res, nchar) - 1))
  #}
  #else {
  #  sapply(x, as.character, pretty)
  #  #res <- as.character.default(x)
  #  #res <- replaceCommasWithinVectors(res)
  #  ## the following substitutions are performed in reverse order
  #  #paste(
  #  #  gsub(";", ",", # switch semicolons to comma
  #  #  gsub("list", "[", # switch list to [
  #  #  gsub("c", "", # remove c
  #  #  gsub("integer0", "", # remove integer0
  #  #  gsub(")", "", # remove closing brackets
  #  #  gsub("(", "", # remove opening brackets
  #  #  gsub(",", "][", # remove commas
  #  #  gsub(" ", "", res # remove spaces
  #  #, fixed = T), fixed = T), fixed = T), fixed = T), fixed = T), 
  #     fixed = T), fixed = T), fixed = T), 
  #  #"]", sep = "") # add closing ]
  #}
}

#' Heuristic test for pretty-printing.
#' 
#' #' A heuristic test for whether it appears that x is 'pretty-printed'. ie 
#' whether it is of the form [2][][3] (printed-printed) or 
#' "c(2L), integer(0), c(3L)" (not pretty-printed)
#' 
#' @param x A character vector defining a 'parental' object
#' @return A logical of length 1. Returns true if it appears x is 
#'   pretty-printed.
#' @export
#' @examples
#' seemsPretty("integer(0),1,c(1,2,4),3")
#' seemsPretty("[][1][2]")
seemsPretty <- function(x){
  isTRUE(substring(x, 1, 1) == "[")
}



#replaceCommasWithinVectors <- function(input){
#  prev <- input
#  current <- gsub("(?<=c\\()([^\\)]+?),([^\\)]+?)(?=\\))", "\\1;\\2", 
#         prev, perl = TRUE)
#  while (any(current != prev)){
#    prev <- current
#    current <- gsub("(?<=c\\()([^\\)]+?),([^\\)]+?)(?=\\))", "\\1;\\2", 
#         prev, perl = TRUE)
#  }
#  current
#}
#

#' Convert a 'parental' object to a 'graphNEL' object.
#' 
#' graphNEL objects are from bioconductor package 'graph'
#'
#' Note that graphNEL's edge list are *children* lists.
#' 
#' @param x An object of class 'parental'
#' @param ... unused
#' @return An object of class 'graphNEL'.
#' @S3method as.graph parental
#' @method as.graph parental
#' @examples
#' if (require(graph)){
#'   x <- bn(integer(0), 1, 2)
#'   y <- as.graph(x)
#'   edges(y)
#' }
as.graph.parental <- function(x, ...){
  require(graph)
  x <- getChildren(x) # convert to children list
  nodeNames <- as.character(seq_along(x))
  edL <- lapply(x, as.character)
  edL <- lapply(edL, function(adj) list(edges = adj))
  names(edL) <- nodeNames
  new("graphNEL", nodes = nodeNames, edgeL = edL, edgemode = "directed")
}

#' Convert a graphNEL object to a 'parental' object.
#' 
#' graphNEL objects are from bioconductor package 'graph'
#'
#' Note that graphNEL's edge list are *children* lists.
#' 
#' @param x An object of class 'graphNEL'
#' @param ... Further arguments (unused)
#' @return An object of class 'parental'.
#' @S3method as.parental graphNEL
#' @method as.parental graphNEL
#' @examples
#' if (require(graph)){
#'   set.seed(123)
#'   c1 <- c(1, 1, 2, 4)
#'   names(c1) <- as.character(1:4)
#'   g1 <- randomNodeGraph(c1)
#'   as.parental(g1)
#' }
as.parental.graphNEL <- function(x, ...){
  edL <- graph::edges(x)
  edL <- lapply(edL, factor, levels = names(edL))
  edL <- unname(edL)
  edL <- lapply(edL, as.integer)
  edL <- lapply(edL, sort.int)
  class(edL) <- "parental"
  edL <- getChildren(edL) # convert to parent list
  class(edL) <- "parental"
  edL
}

#' Convert a graphNEL object to a 'bn' object.
#' 
#' graphNEL objects are from bioconductor package 'graph'
#'
#' Note that graphNEL's edge list are *children* lists.
#' 
#' @param x An object of class 'graphNEL'
#' @param ... unused
#' @return An object of class 'bn'.
#' @S3method as.bn graphNEL
#' @method as.bn graphNEL
#' @examples
#' if (require(graph)){
#'   set.seed(123)
#'   c1 <- c(1, 1, 2, 4)
#'   names(c1) <- as.character(1:4)
#'   g1 <- randomNodeGraph(c1)
#'   as.parental(g1)
#' }
as.bn.graphNEL <- function(x, ...){
  res <- as.parental(x)
  if (!checkAcyclic(res)){
    stop("Not acyclic")
  }
  class(res) <- c("bn", "parental")
  res
}

#' Convert an object a 'bvsresponse' object.
#' 
#' A generic
#' 
#' @param x An object
#' @param ... further arguments passed to method
#' 
#' @export
as.bvsresponse <- function(x, ...){
  UseMethod("as.bvsresponse")
}

#' Convert a 'bvs' object to a 'bvsresponse' object.
#' 
#' A 'bvs' object is a full 'parental' object, whereas a 'bvsresponse' 
#' object is smaller object designed specifically for fast MCMC iterations.
#' 
#' @param x An object of class 'bvsresponse'
#' @param response An integer of length 1 specifying which node is 
#'   the response. This is not required, except for the empty graph.
#' @param ... Further arguments (unused)
#' @return An object of class 'bvsresponse'.
#' @S3method as.bvsresponse bvs
#' @method as.bvsresponse bvs
as.bvsresponse.bvs <- function(x, response, ...){
  stopifnot("bvs" %in% class(x),
            is.valid(x))
  
  if (missing(response)){
    response <- which(sapply(x, length) > 0)
    if (length(response) == 0){
      err <- paste("Could not guess which node is the response,", 
                   "please provide 'response' as an argument.")
      stop()
    }
  }
  bvsresponse(x = x[[response]],
              response = response,
              nNodes = length(x))
}

#' Convert character vector to 'bvsresponse'.
#' 
#' Convert a character vector (a comma-separated string) specifying the 
#' parents of a response to a 'bvsresponse' object.
#' 
#' For example, as.bvsresponse("1,2,3", 4, 5) returns a 'bvsresponse' object 
#' with 5 nodes, with node 4 as response, and nodes 1, 2 and 3 as its 
#' parents.
#' 
#' @param x A character string (possibly vector), a comma-separated list of 
#'   the parents of the response.
#' @param response An integer of length 1 specifying which node 
#'   is the response.
#' @param nNodes An integer of length 1 specifying the number of nodes in 
#'   the variable selection.
#' @param ... Further arguments (unused)
#'
#' @return If length(x) == 1:
#'     An object of class 'bvsresponse'.
#'   If length(x) > 1:
#'     A list of class "bvsresponse.list" containing a number of 
#'     'bvsresponse' objects
#' @S3method as.bvsresponse character
#' @method as.bvsresponse character
as.bvsresponse.character <- function(x, response, nNodes, ...){
  stopifnot("character" %in% class(x),
            is.numeric(response),
            length(response) == 1,
            is.numeric(nNodes),
            length(nNodes) == 1)
  
  res <- lapply(x, function(parents){
    parents <- eval(parse(text = paste("c(", parents, ")")))
    parents <- as.integer(parents)
    parents <- sort.int(parents)
    bvsresponse(parents, response, nNodes)
  })
  
  if (length(x) == 1){
    res <- unlist(res, recursive = F)
    class(res) <- "bvsresponse"
  }
  else {
    class(res) <- "bvsresponse.list"
  }
  res
}

#' Convert 'bvsresponse' to character vector.
#' 
#' Convert a 'bvsresponse' object to a character string. Note that the 
#' number of nodes, and which node the response is not contained in the 
#' resulting character string.
#' 
#' For example, as.character() on a bvsresponse with 10 nodes, response 
#' node 5, which has parents 2, 3, 6, and 10 returns "2,3,6,10".
#' 
#' @param x An object of class 'bvsresponse'
#' @param ... Further arguments (unused)
#' @return An object of class 'parental'.
#' @S3method as.character bvsresponse
#' @method as.character bvsresponse
as.character.bvsresponse <- function(x, ...){
  stopifnot("bvsresponse" %in% class(x))
  
  paste(x$parents, sep = "", collapse = ",")
}

#' Convert 'bvsresponse list' to character vector.
#' 
#' Convert a 'bvsresponse.list' object to a character string. Note that the 
#' number of nodes, and which node the response is not contained in the 
#' resulting character string. 
#'
#' This function passes the list to as.character.bvsresponse()
#' 
#' @param x An object of class 'bvsresponse.list'
#' @param ... Further arguments (unused)
#' @return A character vector, each component of which is a character 
#'   representation of the parents of each 'bvsresponse' included in x.
#' @S3method as.character bvsresponse.list
#' @method as.character bvsresponse.list
as.character.bvsresponse.list <- function(x, ...){
  stopifnot(class(x) == "bvsresponse.list")
  sapply(x, as.character)
}

#' Convert objects to a bvs.
#' 
#' A generic function
#' 
#' @param x An object 
#' @param ... further arguments passed to methods
#' 
#' @export
as.bvs <- function(x, ...){
  UseMethod("as.bvs")
}

#' Convert 'bvsresponse' to 'bvs'.
#' 
#' Convert a 'bvsresponse' object to a 'bvs' object. A 'bvs' object is a 
#' full 'parental' object, whereas a 'bvsresponse' object is smaller 
#' object designed specifically for fast MCMC iterations.
#' 
#' @param x An object of class 'bvsresponse'
#' @param ... further arguments (unused)
#' @return An object of class 'bvs'.
#' @S3method as.bvs bvsresponse
#' @method as.bvs bvsresponse
as.bvs.bvsresponse <- function(x, ...){
  stopifnot(class(x) == "bvsresponse")
  
  res <- empty(x$nNodes, class = "bvs")
  res[[x$response]] <- x$parents
  res
}
