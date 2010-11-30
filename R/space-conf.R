# returns a list of every Sach's confounder network
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

