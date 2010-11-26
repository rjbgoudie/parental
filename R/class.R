#' Constructor function for a \code{parental} object.
#'
#' Objects of class \code{parental} are lists with the ith component specifying
#' the parents of node i. The parents must be specified as a vector of 
#' integers -- the storage.mode() of these integers MUST be integer. 
#' If the node has no parents, use integer(0).
#'
#' Note, in particular, that \code{list(2, 1)} would not be a valid \code{parental} 
#' object, because storage.mode(2) and \code{storage.mode(1) == "double"}
#' Instead one would use \code{list(2L, 1L)}, as documented in the R FAQ (see [1])
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
#' @examples
#' parental(c(), 1, 2)
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
#' \code{bn} is a subclass of \code{parental}. See \code{\link{parental}} for detailed 
#' documentation.
#' 
#' @param ...  A series of vectors specifying the parents of each node. These 
#'   vectors must be of storage.mode "integer".
#' @return An object of class 'bn'.
bn <- function(...){
  parents <- list(...)
  parents <- lapply(parents, as.integer)
  parents <- lapply(parents, sort.int)
  class(parents) <- c("bn", "parental")
  parents
}

#' Constructor function for a 'bvs' object.
#' 'bvs' is a subclass of \code{parental}. See \code{\link{parental}} for detailed 
#' documentation.
#' 
#' @param ... A series of vectors specifying the parents of each node. These 
#'   vectors must be of storage.mode "integer".
#' @return An object of class 'bvs'.
bvs <- function(...){
  parents <- list(...)
  parents <- lapply(parents, as.integer)
  parents <- lapply(parents, sort.int)
  class(parents) <- c("bvs", "bn", "parental")
  parents
}

#' Constructor function for a 'parental.list' object
#' 
#' @param ...  A series of objects of class \code{parental}.
#' @return An object of class 'parental.list'
parental.list <- function(...){
  parentallist <- list(...)
  class(parentallist) <- c("parental.list")
  parentallist
}

"[.parental.list" <- function(x, i){
  x <- unclass(x)[i]
  class(x) <- "parental.list"
  x
}

#' Constructor function for a 'bn.list' object
#' 
#' @param ... A series of objects of class 'bn'.
#' @return An object of class 'bn.list'
bn.list <- function(...){
  bnlist <- list(...)
  class(bnlist) <- c("bn.list", "parental.list")
  bnlist
}

"[.bn.list" <- function(x, i){
  x <- unclass(x)[i]
  class(x) <- c("bn.list", "parental.list")
  x
}

#' Constructor function for a 'bvsresponse.list' object
#' 
#' @param ... A series of objects of class 'bvsresponse'.
#' @return An object of class 'bvsresponse.list'
bvsresponse.list <- function(...){
  bvsrlist <- list(...)
  class(bvsrlist) <- "bvsresponse.list"
  bvsrlist
}

"[.bvsresponse.list" <- function(x, i){
  x <- unclass(x)[i]
  class(x) <- "bvsresponse.list"
  x
}

is.valid <- function(x){
  UseMethod("is.valid")
}

#' Checks whether the supplied \code{parental} is valid.
#' Tests that the parents are sorted correctly, are of 
#' storage.mode() == "integer"
#' 
#' @param x A object of class \code{parental}
#'
#' Returns:
#'   A logical of length 1 indicating whether x is a valid \code{parental} object
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

#' Checks whether the supplied 'bvs' is valid.
#' Tests that the parents are sorted correctly, are of 
#' storage.mode() == "integer".
#' Additionally tests that there is a unique response.
#' 
#' @param x A object of class 'bvs'
#' @return A logical of length 1 indicating whether x is a valid 'bvs' object
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

#' Checks whether the supplied 'bn is valid.
#' Tests that the parents are sorted correctly, are of 
#' storage.mode() == "integer".
#' Additionally checks that the supplied bn is acyclic.
#' 
#' @param x A object of class 'bn'
#' @return A logical of length 1 indicating whether x is a valid 'bn' object
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

#' Concatenates a 'parental.list' object to the console.
#' 
#' @param ... Any number of 'parental.list' objects
#' @return An new 'parental.list' object, including all the supplied parental.lists
c.parental.list <- function(...){
  out <- NextMethod("c")
  class(out) <- "parental.list"
  out
}

#' Concatenates a 'bn.list' object to the console.
#' 
#' @param ... Any number of 'bn.list' objects
#' @return An new 'bn.list' object, including all the supplied bn.lists
c.bn.list <- function(...){

  out <- NextMethod("c")
  class(out) <- c("bn.list", "parental.list")
  out
}

#' Prints a 'parental.list' object to the console.
#' 
#' @param x A 'parental.list' object
#' @return Prints the 'parental.list' object to the console.
print.parental.list <- function(x){

  print(unlist(lapply(x, as.character, pretty = T)))
}

#' Prints a \code{parental} object to the console.
#' 
#' @param x A \code{parental} object
#' @return Prints the \code{parental} object to the console.
print.parental <- function(x){
  print(as.character(x, pretty = T))
}

#' Prints a 'bn' object to the console.
#' 
#' @param x A 'bn' object
#' @return Prints the 'bn' object to the console.
print.bn <- function(x){
  print(as.character(x, pretty = T))
}


renameNodes <- function(...){
  UseMethod("renameNodes")
}

#' Return the parental list, with nodes in each parental
#' in the parental.list renamed to the newnames.
#' 
#' @param x A parental.list
#' @param newnames A character vector specifying the new names for the nodes
#'
#' @return The parental.list with renamed nodes
renameNodes.parental.list <- function(x, newnames){
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

#' Returns an empty graph with n nodes, of the given class
#' 
#' @param n A integer of length 1, specifying the number of nodes
#' @param class The class of the returned graph. Can be any of "parental", "bn" or "bvs".
#' @return An empty graph of the specified class.
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

#' Checks whether the supplied 'bvsresponse' is valid.
#' Tests that the parents are sorted correctly, are of 
#' storage.mode() == "integer".
#' Additionally tests the parents all exist.
#' 
#' @param x: A object of class 'bvsresponse'
#' @return A logical of length 1 indicating whether x is a valid 'bvsresponse' object
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

# Constructor function for a 'bvsresponse' object.
# Objects of class 'bvsresponse' are lists, with the following components:
# 
# $parents: A vectors of integers, specifying the parents of the response. 
# The parents must be specified as a vector of integers -- the 
# storage.mode() of these integers MUST be integer. If the node has no 
# parents, use integer(0).
#
# Note, in particular, that c(2, 1) would not be a valid specification 
# of 'parents', because storage.mode(2) and storage.mode(1) == "double"
# Instead one would use c(2L, 1L), as documented in the R FAQ (see [1])
#
# Parents of each node MUST be sorted in increasing order.
# Conformance with object requirements can be tested by is.valid()
# 
# [1] http://cran.r-project.org/doc/manuals/R-lang.html#Constants
# 
# @param x A vector of integers specifying the parents of the response.
# @param response A integer of length 1 specifying which node (column) is the response.
# @param nNodes The number of nodes (columns) in the variable selection.
# @return An object of class 'bvsresponse'.
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

#' Prints a 'bvsresponse' object to the console.
#' 
#' @param x:        A 'bvsresponse' object
#' @return Prints the 'bvsresponse' object to the console.
print.bvsresponse <- function(x){
  cat("bvsresponse. Col ", x$response, " is response. ", sep = "")
  cat(x$nNodes, " variables. ", sep = "")
    if (length(x$parents) > 0){
    cat("Parents of response: ", as.character(x), ". ", sep = "")
  }
  else {
    cat("Response has no parents")
  }
}

parents <- function(x, i){
  x[[i]]
}

