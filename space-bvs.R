# returns a list of every Bayesian Variable Selection network, 
# with a particular response

enumerateBVSSpace <- function(numberOfNodes, response, 
                              maxNumberParents = numberOfNodes - 1){
  if (length(numberOfNodes) != 1 | length(response) != 1){
    stop("numberOfNodes and response must be of length 1")
  }
  
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5){
    abs(x - round(x)) < tol
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
  
  nodesSeq <- seq_len(numberOfNodes)
  if (maxNumberParents > numberOfNodes - 1){
    maxNumberParents = numberOfNodes - 1
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
      choices <- setdiff(nodesSeq, response)
      
      # combn treats single integers as implying 1:x
      # use a list to force it not to
      # but have to unlist twice in this case
      ul <- function(l) unlist(l, recursive = F)
      if (length(choices) == 1){
        choices <- list(choices)
        ul <- function(l) unlist(unlist(l, recursive = F), recursive = F)
      }
      
      out <- ul(apply(combn(choices, numberOfParents), 2, list))
    }
    ), 
  recursive = F)
  
  # add a NULL to make the empty graph
  length(possibleParents) <- length(possibleParents) + 1
  
  # handle edge case of maxNumberParents == 0
  if (length(possibleParents) == 0) possibleParents <- list(possibleParents)
  
  # empty graph to which parents are added
  empty <- lapply(nodesSeq, function(node) integer(0))
  
  out <- lapply(possibleParents, function(parents) {
    
    # empty[[x]] <- NULL removes that element
    # so avoid this problem
    if (!is.null(parents)){
      empty[[response]] <- parents
    }
    class(empty) <- c("bvs", "bn", "parental")
    empty
  })
  
  class(out) <- c("bvs.list", "bn.list", "parental.list")
  out
}

