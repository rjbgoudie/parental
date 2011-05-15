# Part of the "parental" package, http://github.com/rjbgoudie/parental
# 
# This software is distributed under the GPL-3 license.  It is free,
# open source, and has the attribution requirements (GPL Section 7) in
#   http://github.com/rjbgoudie/parental
# 
# Note that it is required that attributions are retained with each function.
#
# Copyright 2008 Robert J. B. Goudie, University of Warwick

#' Enumerate the space of Confounder Graphs.
#'
#' Enumerate (ie make a list of, not just count) the entire space of 
#' Confounder models, on a given number of nodes, for a 
#' particular response.
#' 
#' @param numberOfConfounders An integer of length 1. The number of 
#'   confounders
#' @param response An integer of length 1. Which node is the response.
#' @param effect ?
#' @param maxNumberParents The maximum number of parents of node response.
#' @return An object of class "parental.list", a list of objects of class 
#'   "parental"
#' @export
enumerateConfSpace <- function(numberOfConfounders, response, effect,
                               maxNumberParents = numberOfConfounders - 2){
  nodesSeq <- seq_len(numberOfConfounders)
  if (maxNumberParents > numberOfConfounders - 1){
    maxNumberParents = numberOfConfounders - 1
    warning("maxNumberParents is too high, using maxNumberParents = ",
            maxNumberParents)
  }
  if (maxNumberParents < 0){
    stop("maxNumberParents must be positive")
  }
  
  allowedNumberOfParents <- seq_len(maxNumberParents)
  
  # all combinations of all lengths
  # unlisting is unelegant, but does the job
  possibleParents <- unlist(lapply(allowedNumberOfParents, 
    function(numberOfParents){
      choices <- setdiff(nodesSeq, c(response, effect))
      if (length(choices) == 1){
        choices <- list(choices)
      }
      unlist(apply(combn(choices, numberOfParents), 2, list), rec = F)
    }
    ), 
  recursive = F)
  
  # add a NULL to make the empty graph
  length(possibleParents) <- length(possibleParents) + 1
  
  # handle edge case of maxNumberParents == 0
  if (length(possibleParents) == 0) possibleParents <- list(possibleParents)
  
  # empty graph to which parents are added
  empty <- lapply(nodesSeq, function(node) integer(0))
  
  withoutLink <- lapply(possibleParents, function(parents) {
    
    # empty[[x]] <- NULL removes that element
    # so avoid this problem
    if (!is.null(parents)){
      empty[[response]] <- parents
      empty[[effect]] <- parents
    }
    empty
  })
  
  withLink <- lapply(possibleParents, function(parents) {
    
    # empty[[x]] <- NULL removes that element
    # so avoid this problem
    if (!is.null(parents)){
      empty[[response]] <- parents
      empty[[effect]] <- parents
    }
    empty[[response]] <- sort.int(c(empty[[response]], effect))
    empty
  })
  
  c(withLink, withoutLink)
}

