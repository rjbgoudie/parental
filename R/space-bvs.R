# returns a list of every Bayesian Variable Selection network, 
# with a particular response

#' Enumerate the space of BVSs
#' 
#' Enumerate (ie make a list of, not just count) the entire space of 
#' Bayesian Variable Selection models, on a given number of nodes, for a 
#' particular response. Optionally, restrcit the maximum number of parents
#'
#' @param numberOfNodes An integer of length 1. The number of noes
#' @param response An integer of length 1. Which node is the response
#' @param maxNumberParents The maximum number of parents of node response.
#' @return An object of class "bvs.list", a list of objects of class "bvs"
#' @export
enumerateBVSSpace <- function(numberOfNodes, response, 
                              maxNumberParents = numberOfNodes - 1){
  if (length(numberOfNodes) != 1 | length(response) != 1){
    stop("numberOfNodes and response must be of length 1")
  }
  
  if (!is.wholenumber(numberOfNodes) || numberOfNodes <= 0){
    stop("Number of nodes must be a positive natural number")
  }
  if (!is.wholenumber(response) || response <= 0){
    stop("Response must be a positive natural number")
  }
  if (response > numberOfNodes){
    stop("response must be a node")
  }
  
  if (maxNumberParents > numberOfNodes - 1){
    maxNumberParents = numberOfNodes - 1
    warning("maxNumberParents is too high, using maxNumberParents = ", 
            maxNumberParents)
  }
  if (maxNumberParents < 0){
    stop("maxNumberParents must be positive")
  }
  
  nodesSeq <- seq_len(numberOfNodes)
  choices <- setdiff(nodesSeq, response)
  possibleParents <- enumerateParents(potentialParents = choices, 
                                      maxNumberParents)
  
  # empty graph to which parents are added
  empty <- lapply(nodesSeq, function(node) integer(0))
  
  if (maxNumberParents > 0){
    out <- lapply(possibleParents, function(parents) {
  
      # empty[[x]] <- NULL removes that element
      # so avoid this problem
      if (!is.null(parents)){
        empty[[response]] <- parents
      }
      class(empty) <- c("bvs", "bn", "parental")
      empty
    })
  } else {
    class(empty) <- c("bvs", "bn", "parental")
    out <- list(empty)
  }
  
  class(out) <- c("bvs.list", "bn.list", "parental.list")
  out
}

#' Enumerate all the combinations of possible parents, given a SET of 
#' possible parents. Optionally, restrcit the maximum number of parents. 
#' Optionally, require that a particular parent is always present.
#'
#' @param potentialParents A numeric vector of possible parents.
#'                     Note: at the moment, this should EXCLUDE any 
#'                     required parents.
#' @param maxNumberParents The maximum number of parents of node response.
#' @param required A numeric vector of required parents
#' @return A list of possible parents sets, each of which will be a sorted 
#'   numeric vector, stored as integers.
#' @export
enumerateParents <- function(potentialParents,
                             maxNumberParents = length(potentialParents),
                             required = integer(0)){
  stopifnot(class(potentialParents) %in% c("numeric", "integer"),
            length(potentialParents) >= 0,
            sum(duplicated(potentialParents)) == 0,
            #maxNumberParents <= length(potentialParents),
            !any(required %in% potentialParents))
  if (length(potentialParents) == 0){
    possibleParents <- list()
  }
  else {
    allowedNumberOfParents <- min(maxNumberParents, length(potentialParents))
    allowedNumberOfParentsSeq <- seq_len(allowedNumberOfParents)
  
    # all combinations of all lengths
    # unlisting is unelegant, but does the job
    possibleParents <- unlist(lapply(allowedNumberOfParentsSeq, 
      function(numberOfParents){
        # combn treats single integers as implying 1:x
        # use a list to force it not to
        # but have to unlist twice in this case
        ul <- function(l) unlist(l, recursive = F)
        if (length(potentialParents) == 1){
          potentialParents <- list(potentialParents)
          ul <- function(l) unlist(unlist(l, recursive = F), recursive = F)
        }
      
        ul(apply(combn(potentialParents, numberOfParents), 2, list))
      }
      ), 
    recursive = F)
  }
  
  # add a NULL to make the empty graph
  length(possibleParents) <- length(possibleParents) + 1

  possibleParents <- lapply(possibleParents, function(out){
    if (!is.null(out)){
      out <- sort.int(c(out, required))
    }
    else {
      out <- required
    }
    class(out) <- "integer"
    out
  })

  # handle edge case of maxNumberParents == 0
  if (length(possibleParents) == 0) possibleParents <- list()

  possibleParents
}
