parental <- function(...){
  # Constructor function for a 'parental' object.
  # Objects of class 'parental' are lists with the ith component specifying 
  # the parents of node i. The parents must be specified as a vector of 
  # integers -- the storage.mode() of these integers MUST be integer. 
  # If the node has no parents, use integer(0).
  #
  # Note, in particular, that list(2, 1) would not be a valid 'parental' 
  # object, because storage.mode(2) and storage.mode(1) == "double"
  # Instead one would use list(2L, 1L), as documented in the R FAQ (see [1])
  #
  # Parents of each node MUST be sorted in increasing order.
  # Conformance with object requirements can be tested by is.valid()
  # 
  # [1] http://cran.r-project.org/doc/manuals/R-lang.html#Constants
  # 
  # Args:
  #   ...:  A series of vectors specifying the parents of each node. These 
  #         vectors must be of storage.mode "integer".
  #
  # Returns:
  #   An object of class 'parental'.
  parents <- list(...)
  parents <- lapply(parents, as.integer)
  parents <- lapply(parents, sort.int)
  class(parents) <- "parental"
  parents
}

bn <- function(...){
  # Constructor function for a 'bn' object.
  # 'bn' is a subclass of 'parental'. See parental() for detailed 
  # documentation.
  # 
  # Args:
  #   ...:  A series of vectors specifying the parents of each node. These 
  #         vectors must be of storage.mode "integer".
  #
  # Returns:
  #   An object of class 'bn'.
  parents <- list(...)
  parents <- lapply(parents, as.integer)
  parents <- lapply(parents, sort.int)
  class(parents) <- c("bn", "parental")
  parents
}

bvs <- function(...){
  # Constructor function for a 'bvs' object.
  # 'bvs' is a subclass of 'parental'. See parental() for detailed 
  # documentation.
  # 
  # Args:
  #   ...:  A series of vectors specifying the parents of each node. These 
  #         vectors must be of storage.mode "integer".
  #
  # Returns:
  #   An object of class 'bvs'.
  parents <- list(...)
  parents <- lapply(parents, as.integer)
  parents <- lapply(parents, sort.int)
  class(parents) <- c("bvs", "bn", "parental")
  parents
}

parental.list <- function(...){
  # Constructor function for a 'parental.list' object
  # 
  # Args:
  #   ...:  A series of objects of class 'parental'.
  #
  # Returns:
  #   An object of class 'parental.list'
  parentallist <- list(...)
  class(parentallist) <- c("parental.list")
  parentallist
}

"[.parental.list" <- function(x, i){
  x <- unclass(x)[i]
  class(x) <- "parental.list"
  x
}

bn.list <- function(...){
  # Constructor function for a 'bn.list' object
  # 
  # Args:
  #   ...:  A series of objects of class 'bn'.
  #
  # Returns:
  #   An object of class 'bn.list'
  bnlist <- list(...)
  class(bnlist) <- c("bn.list", "parental.list")
  bnlist
}

"[.bn.list" <- function(x, i){
  x <- unclass(x)[i]
  class(x) <- c("bn.list", "parental.list")
  x
}

bvsresponse.list <- function(...){
  # Constructor function for a 'bvsresponse.list' object
  # 
  # Args:
  #   ...:  A series of objects of class 'bvsresponse'.
  #
  # Returns:
  #   An object of class 'bvsresponse.list'
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

is.valid.parental <- function(x){
  # Checks whether the supplied 'parental' is valid.
  # Tests that the parents are sorted correctly, are of 
  # storage.mode() == "integer"
  # 
  # Args:
  #   x: A object of class 'parental'
  #
  # Returns:
  #   A logical of length 1 indicating whether x is a valid 'parental' object
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

is.valid.bvs <- function(x){
  # Checks whether the supplied 'bvs' is valid.
  # Tests that the parents are sorted correctly, are of 
  # storage.mode() == "integer".
  # Additionally tests that there is a unique response.
  # 
  # Args:
  #   x: A object of class 'bvs'
  #
  # Returns:
  #   A logical of length 1 indicating whether x is a valid 'bvs' object
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

is.valid.bn <- function(x){
  # Checks whether the supplied 'bn is valid.
  # Tests that the parents are sorted correctly, are of 
  # storage.mode() == "integer".
  # Additionally checks that the supplied bn is acyclic.
  # 
  # Args:
  #   x: A object of class 'bn'
  #
  # Returns:
  #   A logical of length 1 indicating whether x is a valid 'bn' object
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

c.parental.list <- function(...){
  # Concatenates a 'parental.list' object to the console.
  # 
  # Args:
  #   ...:        Any number of 'parental.list' objects
  # 
  # Returns:
  #   An new 'parental.list' object, including all the supplied parental.lists
  out <- NextMethod("c")
  class(out) <- "parental.list"
  out
}

c.bn.list <- function(...){
  # Concatenates a 'bn.list' object to the console.
  # 
  # Args:
  #   ...:        Any number of 'bn.list' objects
  # 
  # Returns:
  #   An new 'bn.list' object, including all the supplied bn.lists
  out <- NextMethod("c")
  class(out) <- c("bn.list", "parental.list")
  out
}

print.parental.list <- function(x){
  # Prints a 'parental.list' object to the console.
  # 
  # Args:
  #   x:        A 'parental.list' object
  # 
  # Returns:
  #   Prints the 'parental.list' object to the console.
  print(unlist(lapply(x, as.character, pretty = T)))
}

print.parental <- function(x){
  # Prints a 'parental' object to the console.
  # 
  # Args:
  #   x:        A 'parental' object
  # 
  # Returns:
  #   Prints the 'parental' object to the console.
  print(as.character(x, pretty = T))
}

print.bn <- function(x){
  # Prints a 'bn' object to the console.
  # 
  # Args:
  #   x:        A 'bn' object
  # 
  # Returns:
  #   Prints the 'bn' object to the console.
  print(as.character(x, pretty = T))
}


renameNodes <- function(...){
  UseMethod("renameNodes")
}

renameNodes.parental.list <- function(x, newnames){
  # Return the parental list, with nodes in each parental
  # in the parental.list renamed to the newnames.
  # 
  # Args:
  #   x:        A parental.list
  #   newnames: A character vector specifying the new names for the nodes
  #
  # Returns:
  #   The parental.list with renamed nodes
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

empty <- function(n, class = "parental", response){
  # Returns an empty graph with n nodes, of the given class
  # 
  # Args:
  #   n:     A integer of length 1, specifying the number of nodes
  #   class: The class of the returned graph. Can be any of "parental",
  #          "bn" or "bvs".
  #
  # Returns:
  #   An empty graph of the specified class.
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

is.valid.bvsresponse <- function(x){
  # Checks whether the supplied 'bvsresponse' is valid.
  # Tests that the parents are sorted correctly, are of 
  # storage.mode() == "integer".
  # Additionally tests the parents all exist.
  # 
  # Args:
  #   x: A object of class 'bvsresponse'
  #
  # Returns:
  #   A logical of length 1 indicating whether x is a valid 'bvsresponse' 
  #   object
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

bvsresponse <- function(x, response, nNodes){
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
  # Args:
  #   x:        A vector of integers specifying the parents of the response.
  #   response: A integer of length 1 specifying which node (column) is the 
  #             response.
  #   nNodes:   The number of nodes (columns) in the variable selection.
  # 
  # Returns:
  #   An object of class 'bvsresponse'.
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

print.bvsresponse <- function(x){
  # Prints a 'bvsresponse' object to the console.
  # 
  # Args:
  #   x:        A 'bvsresponse' object
  # 
  # Returns:
  #   Prints the 'bvsresponse' object to the console.
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

