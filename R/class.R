# Part of the "parental" package, http://github.com/rjbgoudie/parental
# 
# This software is distributed under the GPL-3 license.  It is free,
# open source, and has the attribution requirements (GPL Section 7) in
#   http://github.com/rjbgoudie/parental
# 
# Note that it is required that attributions are retained with each function.
#
# Copyright 2008 Robert J. B. Goudie, University of Warwick

#' The parental package.
#' 
#' Parental
#' 
#' @import lattice grid network
#' @docType package
#' @name package-parental
#' @aliases package-parental
NULL

#' Constructor function for a \code{parental} object.
#'
#' Objects of class \code{parental} are lists with the ith component 
#' specifyingthe parents of node i. The parents must be specified as a 
#' vector of integers -- the storage.mode() of these integers MUST be 
#' integer. If the node has no parents, use integer(0).
#'
#' Note, in particular, that \code{list(2, 1)} would not be a valid 
#' \code{parental}  object, because storage.mode(2) and 
#' \code{storage.mode(1) == "double"}. Instead one would use 
#' \code{list(2L, 1L)}, as documented in the R FAQ (see [1])
#'
#' Parents of each node MUST be sorted in increasing order.
#' Conformance with object requirements can be tested by \code{is.valid()}
#'
#' [1] http://cran.r-project.org/doc/manuals/R-lang.html#Constants
#'
#' @param ... A series of vectors specifying the parents of each node. These 
#'   vectors must be of \code{storage.mode} "integer".
#' @return An object of class \code{parental}.
#' @keywords constructor
#' @export
#' @seealso \code{\link{bn}}
#' @examples
#' parental(c(), 1, 2)
#' parental(3, c(), 1)
#' parental(c(), c(1, 3), c())
parental <- function(...){
  parents <- list(...)
  parents <- lapply(parents, as.integer)
  parents <- lapply(parents, sort.int)
  class(parents) <- "parental"
  parents
}

#' Constructor function for a 'bn' object.
#'
#' \code{bn} is a subclass of \code{parental}. See \code{\link{parental}} 
#' for detailed documentation.
#' 
#' @param ... A series of vectors specifying the parents of each node. 
#'   These vectors must be of storage.mode "integer".
#' @return An object of class 'bn'.
#' @export
#' @seealso \code{\link{parental}}
#' @examples
#' bn(c(), 1, 2)
bn <- function(...){
  parents <- list(...)
  parents <- lapply(parents, as.integer)
  parents <- lapply(parents, sort.int)
  class(parents) <- c("bn", "parental")
  parents
}

#' Constructor function for a 'bvs' object.
#' 
#' 'bvs' is a subclass of \code{parental}. See \code{\link{parental}} 
#' for detailed documentation.
#' 
#' @param ... A series of vectors specifying the parents of each node. 
#'   These vectors must be of storage.mode "integer".
#' @return An object of class 'bvs'.
#' @export
bvs <- function(...){
  parents <- list(...)
  parents <- lapply(parents, as.integer)
  parents <- lapply(parents, sort.int)
  class(parents) <- c("bvs", "bn", "parental")
  parents
}

#' Constructor function for a 'parental list' object.
#' 
#' @param ... A series of objects of class \code{parental}.
#' @return An object of class 'parental.list'
#' @export
#' @seealso \code{\link{parental}}, \code{\link{bn.list}}, 
#'   \code{\link{[.parental.list}}, \code{\link{c.parental.list}}
#' @examples
#' p1 <- parental(c(), c(1), c(1, 2, 4), c(3))
#' p2 <- parental(c(2), c(), c(4), c(2, 3))
#' x <- parental.list(p1, p2)
parental.list <- function(...){
  parentallist <- list(...)
  class(parentallist) <- c("parental.list")
  parentallist
}

#' Extract parts of a 'parental list'.
#' 
#' Operator that acts on objects of class \code{parental.list} to extract 
#' parts of the list
#' 
#' @param x Object from which to extract element(s)
#' @param i Indicies specifying elements to extract
#' @return An object of class \code{parental.list}, consisting the parts of 
#'   \code{x} indicated by \code{i}
#' @S3method "[" parental.list
#' @method "[" parental.list
#' @aliases [.parental.list
#' @usage \method{[}{parental.list}(x, i)
#' @name getpl
#' @seealso \code{\link{parental.list}}, \code{\link{c.parental.list}}
#' @examples
#' p1 <- parental(c(), c(1), c(1, 2, 4), c(3))
#' p2 <- parental(c(2), c(), c(4), c(2, 3))
#' x <- parental.list(p1, p2)
#' x[1]
#' x[[1]]
#' x[2]
"[.parental.list" <- function(x, i){
  x <- unclass(x)[i]
  class(x) <- "parental.list"
  x
}

#' Constructor function for a 'bn list'.
#' 
#' Constructs bn.list objects
#' 
#' @param ... A series of objects of class 'bn'.
#' @return An object of class 'bn.list'
#' @export
#' @seealso \code{\link{[.bn.list}}, \code{\link{c.bn.list}}
#' @examples
#' p1 <- bn(c(), c(1), c(1, 2, 4), c())
#' p2 <- bn(c(2), c(), c(1), c())
#' x <- bn.list(p1, p2)
#' x[1]
#' x[[1]]
#' x[2]
bn.list <- function(...){
  bnlist <- list(...)
  class(bnlist) <- c("bn.list", "parental.list")
  bnlist
}

#' Extract parts of a 'bn list'.
#' 
#' Operator that acts on objects of class \code{bn.list} to extract 
#' parts of the list
#' 
#' @param x Object from which to extract element(s)
#' @param i Indicies specifying elements to extract
#' @return An object of class \code{bn.list}, consisting the parts of 
#'   \code{x} indicated by \code{i}
#' @name getbnl
#' @aliases [.bn.list
#' @usage \method{[}{bn.list}(x, i)
#' @S3method "[" bn.list
#' @method "[" bn.list
#' @seealso \code{\link{bn.list}}
#' @examples
#' p1 <- bn(c(), c(1), c(1, 2, 4), c())
#' p2 <- bn(c(2), c(), c(1), c())
#' x <- bn.list(p1, p2)
#' x[1]
#' x[[1]]
#' x[2]
"[.bn.list" <- function(x, i){
  x <- unclass(x)[i]
  class(x) <- c("bn.list", "parental.list")
  x
}

#' Constructor function for a 'bvsresponse list' object.
#' 
#' @param ... A series of objects of class 'bvsresponse'.
#' @return An object of class 'bvsresponse.list'
#' @export
bvsresponse.list <- function(...){
  bvsrlist <- list(...)
  class(bvsrlist) <- "bvsresponse.list"
  bvsrlist
}

#' Extract parts of a 'bvsresponse list'.
#' 
#' Operator that acts on objects of class \code{bvsresponse.list} to extract 
#' parts of the list
#' 
#' @param x Object from which to extract element(s)
#' @param i Indicies specifying elements to extract
#' @return An object of class \code{bvsresponse.list}, consisting the parts 
#'   of \code{x} indicated by \code{i}
#' @name getbvsrl
#' @aliases [.bvsresponse.list
#' @usage \method{[}{bvsresponse.list}(x, i)
#' @S3method "[" bvsresponse.list
#' @method "[" bvsresponse.list
"[.bvsresponse.list" <- function(x, i){
  x <- unclass(x)[i]
  class(x) <- "bvsresponse.list"
  x
}

#' Check validity.
#' 
#' Checks the validity of objects
#' 
#' @param x Object to check for validity
#' @return A logical indicating validity.
#' @seealso \code{\link{is.valid.parental}},
#'   \code{\link{is.valid.bn}}, \code{\link{is.valid.bvs}},
#'   \code{\link{is.valid.bvsresponse}}
#' @export
is.valid <- function(x){
  UseMethod("is.valid")
}

#' Check validity.
#' 
#' Checks whether the supplied \code{parental} is valid.
#' Tests that the parents are sorted correctly, are of 
#' storage.mode() == "integer"
#' 
#' @param x A object of class \code{parental}
#' @return A logical of length 1 indicating whether x is a valid 
#'   \code{parental} object
#' @S3method is.valid parental
#' @method is.valid parental
#' @seealso \code{\link{is.valid.bn}}
#' @examples
#' x <- parental(c(), c(1), c(1, 2, 4), c(3))
#' is.valid(x)
is.valid.parental <- function(x){
  stopifnot("parental" %in% class(x))
  tryCatch({
    sorted <- all(!sapply(x, is.unsorted)) # check all components sorted
    ints <- all(sapply(x, storage.mode) == "integer") # all ints

    if (all(c(sorted, ints))){
      T
    }
    else {
      F
    }
    },
    error = function(e) F)
}

#' Check validity.
#' 
#' Checks whether the supplied 'bvs' is valid.
#' Tests that the parents are sorted correctly, are of 
#' storage.mode() == "integer".
#' Additionally tests that there is a unique response.
#' 
#' @param x A object of class 'bvs'
#' @return A logical of length 1 indicating whether x is a valid 'bvs' 
#'   object.
#' @S3method is.valid bvs
#' @method is.valid bvs
is.valid.bvs <- function(x){
  stopifnot("bvs" %in% class(x))
  tryCatch({
    sorted <- all(!sapply(x, is.unsorted)) # check all components sorted
    ints <- all(sapply(x, storage.mode) == "integer") # all ints
    # check only one response, or could be the empty graph
    resp <- sum(sapply(x, length) > 0) %in% c(0, 1)
  
    if (all(c(sorted, ints, resp))){
      T
    }
    else {
      F
    }
    },
    error = function(e) F)
}

#' Check validity.
#' 
#' Checks whether the supplied 'bn is valid.
#' Tests that the parents are sorted correctly, are of 
#' storage.mode() == "integer".
#' Additionally checks that the supplied bn is acyclic.
#' 
#' @param x A object of class 'bn'
#' @return A logical of length 1 indicating whether x is a valid 'bn' object
#' @S3method is.valid bn
#' @method is.valid bn
#' @seealso \code{\link{is.valid.parental}}
#' @examples
#' x <- bn(c(), 1, 2)
#' is.valid(x)
is.valid.bn <- function(x){
  stopifnot("bn" %in% class(x),
            "parental" %in% class(x))
  tryCatch({
    sorted <- all(!sapply(x, is.unsorted)) # check all components sorted
    ints <- all(sapply(x, storage.mode) == "integer") # all ints
    acyclic <- checkAcyclic(x)
  
    if (all(c(sorted, ints, acyclic))){
      T
    }
    else {
      F
    }
    },
    error = function(e) F)
}

#' Concatenates 'parental list'.
#' 
#' @param ... Any number of 'parental.list' objects
#' @return An new 'parental.list' object, including all the supplied 
#'   parental.lists
#' @S3method c parental.list
#' @method c parental.list
#' @seealso \code{\link{parental.list}}, \code{\link{[.parental.list}}
#' @examples
#' p1 <- parental(c(), c(1), c(1, 2, 4), c(3))
#' p2 <- parental(c(2), c(), c(4), c(2, 3))
#' x <- parental.list(p1, p2)
#' y <- parental.list(p1, p1)
#' c(x, y)
c.parental.list <- function(...){
  out <- NextMethod("c")
  class(out) <- "parental.list"
  out
}

#' Concatenates 'bn list'.
#' 
#' @param ... Any number of 'bn.list' objects
#' @return An new 'bn.list' object, including all the supplied bn.lists
#' @S3method c bn.list
#' @method c bn.list
#' @seealso \code{\link{bn.list}}, \code{\link{[.bn.list}}
#' @examples
#' p1 <- bn(c(), c(1), c(1, 2, 4), c())
#' p2 <- bn(c(2), c(), c(1), c())
#' x <- bn.list(p1, p2)
#' y <- bn.list(p2, p2, p1)
#' c(x, y)
c.bn.list <- function(...){

  out <- NextMethod("c")
  class(out) <- c("bn.list", "parental.list")
  out
}

#' Prints 'parental list'.
#' 
#' @param x A 'parental.list' object
#' @param ... Further arguments, currently unused.
#' @return Prints the 'parental.list' object to the console.
#' @S3method print parental.list
#' @method print parental.list
#' @seealso \code{\link{parental.list}}
#' @examples
#' p1 <- parental(c(), c(1), c(1, 2, 4), c(3))
#' p2 <- parental(c(2), c(), c(4), c(2, 3))
#' x <- parental.list(p1, p2)
#' x
print.parental.list <- function(x, ...){

  print(unlist(lapply(x, as.character, pretty = T)))
}

#' Prints 'parental'.
#' 
#' @param x A \code{parental} object
#' @param ... Further arguments, currently unused
#' @return Prints the \code{parental} object to the console.
#' @S3method print parental
#' @method print parental
#' @seealso \code{\link{parental}}
#' @examples
#' p1 <- parental(c(), c(1), c(1, 2, 4), c(3))
#' p1
print.parental <- function(x, ...){
  print(as.character(x, pretty = T))
}

#' Prints 'bn'.
#' 
#' @param x A 'bn' object
#' @param ... Further arguments (unused)
#' @return Prints the 'bn' object to the console.
#' @S3method print bn
#' @method print bn
#' @seealso \code{\link{bn}}
#' @examples
#' x <- bn(c(), 1, 2)
#' x
print.bn <- function(x, ...){
  print(as.character(x, pretty = T))
}

#' Rename nodes.
#' 
#' A generic for renaming nodes
#' 
#' @param x Object to rename
#' @param ... Further arguments, passed to method
#' @seealso \code{\link{renameNodes.parental.list}}
#' @export
renameNodes <- function(x, ...){
  UseMethod("renameNodes")
}

#' Rename nodes.
#' 
#' Return the parental list, with nodes in each parental
#' in the parental.list renamed to the newnames.
#' 
#' @param x A parental.list
#' @param newnames A character vector specifying the new names for the 
#'   nodes
#' @param ... Further arguments, currently unused
#'
#' @return The parental.list with renamed nodes
#' @S3method renameNodes parental.list
#' @method renameNodes parental.list
renameNodes.parental.list <- function(x, newnames, ...){
  stopifnot(
    "parental.list" %in% class(x),
    class(newnames) == "character",
    all(length(newnames) == sapply(x, length)) # the number of names match 
                                               # number of nodes
  )
  
  oldclass <- class(x)
  res <- lapply(x, function(parental){
    names(parental) <- newnames
    parental
  })
  class(res) <- oldclass
  res
}


#' Return a complete graph.
#'
#' @param n the number of nodes
#' @return A complete \code{parental} graph
#' @export
#' @seealso \code{\link{empty}}, \code{\link{parental}}
#' @examples
#' complete(5)
complete <- function(n){
  stopifnot(class(n) %in% c("numeric", "integer"),
            n >= 1,
            length(class) == 1)
  nodeSeq <- seq_len(n)
  res <- do.call(list, lapply(seq_len(n), function(i){
    as.integer(sort.int(setdiff(nodeSeq, i)))
  }))
  class(res) <- "parental"
  res
}

#' Return empty graph.
#' 
#' Returns an empty graph with n nodes, of the given class
#' 
#' @param n A integer of length 1, specifying the number of nodes
#' @param class The class of the returned graph. Can be any of "parental", 
#'   "bn", "bvs", or "bvsresponse".
#' @param response For "bvsresponse", which node is the response
#' @return An empty graph of the specified class.
#' @export
#' @seealso \code{\link{complete}}, \code{\link{parental}}, \code{\link{bn}}
#' @examples
#' empty(5)
#' empty(5, "bn")
empty <- function(n, class = "parental", response){
  stopifnot(class(n) %in% c("numeric", "integer"),
            n >= 1,
            length(class) == 1,
            class %in% c("parental", "bn", "bvs", "bvsresponse"))
  
  if (class == "parental"){
    outclass <- "parental"
  }
  else if (class == "bn"){
    outclass <- c("bn", "parental")
  }
  else if (class == "bvs"){
    outclass <- c("bvs", "bn", "parental")
  }
  else if (class == "bvsresponse"){
    stopifnot(!missing(response))
    outclass <- "bvs" # temporary to allow method dispatch as.bvsresponse
  }
  
  res <- do.call(list, lapply(seq_len(n), function(i){
    integer(0)
  }))
  class(res) <- outclass
  if (class == "bvsresponse"){
    res <- as.bvsresponse(res, response = response)
  }
  res
}

#' Checks validity.
#' 
#' Checks whether the supplied 'bvsresponse' is valid.
#' Tests that the parents are sorted correctly, are of 
#' storage.mode() == "integer".
#' Additionally tests the parents all exist.
#' 
#' @param x A object of class 'bvsresponse'
#' @return A logical of length 1 indicating whether x is a valid 
#'   'bvsresponse' object
#' @S3method is.valid bvsresponse
#' @method is.valid bvsresponse
is.valid.bvsresponse <- function(x){
  stopifnot("bvsresponse" %in% class(x))
  
  if (any(is.null(x$parents), is.null(x$response), is.null(x$nNodes))){
    F
  }
  else {
    if (!all(length(x$nNodes) == 1, length(x$response) == 1)){
      F
    }
    else {
      sorted <- !is.unsorted(x$parents) # check all components sorted
      ints <- storage.mode(x$parents) == "integer" # all ints
      maxNotTooHigh <- length(x$parents) == 0 || all(x$parents <= x$nNodes)
      minNotTooLow <- length(x$parents) == 0 || all(x$parents >= 1L)
      responseWhole <- round(x$response) == x$response
      nNodesWhole <- round(x$nNodes) == x$nNodes
      responseNotInParents <- !x$response %in% x$parents
      responseNotTooHigh <- x$response <= x$nNodes
    
      isvalid <- all(c(sorted, ints, maxNotTooHigh, minNotTooLow,
                       responseWhole, nNodesWhole, responseNotInParents))
      if (isvalid){
        T
      }
      else {
        F
      }
    }
  }
}

#' Construct 'bvsreponse'.
#' 
#' Constructor function for a 'bvsresponse' object.
#' Objects of class 'bvsresponse' are lists, with the following components:
#' 
#' $parents: A vectors of integers, specifying the parents of the response. 
#' The parents must be specified as a vector of integers -- the 
#' storage.mode() of these integers MUST be integer. If the node has no 
#' parents, use integer(0).
#'
#' Note, in particular, that c(2, 1) would not be a valid specification 
#' of 'parents', because storage.mode(2) and storage.mode(1) == "double"
#' Instead one would use c(2L, 1L), as documented in the R FAQ (see [1])
#'
#' Parents of each node MUST be sorted in increasing order.
#' Conformance with object requirements can be tested by is.valid()
#' 
#' [1] http://cran.r-project.org/doc/manuals/R-lang.html#Constants
#' 
#' @param x A vector of integers specifying the parents of the response.
#' @param response A integer of length 1 specifying which node (column) 
#'   is the response.
#' @param nNodes The number of nodes (columns) in the variable selection.
#' @return An object of class 'bvsresponse'.
#' @export
bvsresponse <- function(x, response, nNodes){
  stopifnot(class(x) == "integer",
            is.numeric(x),
            length(response) == 1,
            is.numeric(nNodes),
            length(nNodes) == 1,
            all(x <= nNodes),
            all(x >= 1),
            !response %in% x)
  
  res <- list(parents  = x,
              response = as.integer(response),
              nNodes   = as.integer(nNodes))
  class(res) <- "bvsresponse"
  res
}

#' Prints 'bvsresponse'.
#' 
#' Prints a 'bvsresponse' object to the console.
#' 
#' @param x A 'bvsresponse' object
#' @param ... Further arguments, currently unused
#' @return Prints the 'bvsresponse' object to the console.
#' @S3method print bvsresponse
#' @method print bvsresponse
print.bvsresponse <- function(x, ...){
  cat("bvsresponse. Col ", x$response, " is response. ", sep = "")
  cat(x$nNodes, " variables. ", sep = "")
    if (length(x$parents) > 0){
    cat("Parents of response: ", as.character(x), ". ", sep = "")
  }
  else {
    cat("Response has no parents")
  }
}

#' Parents.
#'
#' @param x The object for which to get the parents
#' @param i Which node
#' @return The parents
#' @export
#' @examples
#' x <- parental(c(), 1, c(2, 4), c())
#' parents(x, 3)
parents <- function(x, i){
  x[[i]]
}

