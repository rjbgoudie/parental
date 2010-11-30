#' @export
sampleBN <- function(n, maxNumberParents = NULL){
  nodeSeq <- seq_len(n)
  
  # sample an order
  ordering <- sample(nodeSeq, replace = F)
  
  out <- lapply(nodeSeq, function(i){
    rank <- which(ordering == i)
    if (rank > 1){
      potentialParents <- ordering[seq_len(rank - 1)]
      numberOfPotentialParents <- length(potentialParents)
      
      if (!is.null(maxNumberParents) && 
          numberOfPotentialParents > maxNumberParents){
        numberOfPotentialParents <- maxNumberParents
      }
      
      numberOfParents <- sample.int(numberOfPotentialParents + 1, size = 1)
      numberOfParents <- numberOfParents - 1
      
      if (numberOfPotentialParents > 1){
        out <- sort.int(sample(potentialParents, size = numberOfParents, replace = F))
        storage.mode(out) <- "integer"
        out
      }
      else if (numberOfPotentialParents == 1){
        out <- sort.int(potentialParents[sample(c(T, F), size = numberOfParents)])
        storage.mode(out) <- "integer"
        out
      }
      else {
        stop("Unexpected")
      }
    }
    else {
      integer(0)
    }
  })
  class(out) <- c("bn", "parental")
  out
}

#' @export
sampleBN2 <- function(n, k){
  # hypercube
  # the higher k the sparser the partial order
  
  x <- vapply(seq_len(n), function(x) runif(k), numeric(k))
  out <- empty(n, class = "bn")
  for (i in seq_len(n)){
    for (j in setdiff(seq_len(n), i)){
      # an arrow i -> j  if for all k, i < j
      
      if (k == 1){
        if (all(x[i] < x[j])){
          out[[j]] <- sort.int(c(out[[j]], i))
        }
      } else {
        if (all(x[, i] < x[, j])){
          out[[j]] <- sort.int(c(out[[j]], i))
        }
      }
    }
  }
  out
}

#' @export
expected.sample <- function(n, size, replace, prob){
  exp <- prob * size
  out <- rep(seq.int(n), exp)
  shortage <- size - length(out)
  if (shortage > 0){
    shortfall <- exp - floor(exp)
    whichGreatestShortfall <- order(shortfall, decreasing = T)[1]
    out <- c(rep(whichGreatestShortfall, shortage), out)
    out <- sort.int(out)
  }
  out
}

##### sensitivity to the order in which parents are specified
#' @export
simulate.bn <- function(bn, ptables, N, expectation = F){
  stopifnot(
    "bn" %in% class(bn),
    class(expectation) == "logical",
    length(expectation) == 1,
    length(N) == 1,
    class(N) %in% c("integer", "numeric"),
    class(ptables) == "list",
    all(sapply(ptables, class) == "table")
  )
  if (isTRUE(expectation)){
    creator <- expected.sample
  }
  else {
    creator <- sample.int
  }
  
  nNodes <- length(bn)
  numberOfParents <- sapply(bn, length)
  dat <- as.data.frame(matrix(NA, ncol = nNodes, nrow = N))
  
  # topologically sort
  nodeOrder <- topologicallyOrder(bn)
  
  numberOfLevelsAll <- lapply(seq_len(nNodes), function(i){
    dim(ptables[[i]])[1]
  })
  
  # loop through the nodes in order
  for (i in nodeOrder){
    ptable <- ptables[[i]]
    
    # first dimension is the values
    numberOfLevels <- numberOfLevelsAll[[i]]
    
    if (numberOfParents[i] == 0){
      dat[, i] <- creator(numberOfLevels,
                             size    = N,
                             replace = T,
                             prob    = ptable)
    }
    else {
      parents <- bn[[i]]
      
      local <- dat[, parents, drop = F]
      local <- data.frame(lapply(seq_len(ncol(local)), function(i){
        ls <- seq_len(numberOfLevelsAll[[parents[i]]])
        factor(local[, i], levels = ls)
      }))
      
      # find all the configurations of the parents that exist
      tab <- table(local)
      whichTab <- which(tab > 0, arr.ind = T)
      
      # loop over the configurations
      for (rowNum in seq.int(nrow(whichTab))){
        config <- whichTab[rowNum, ]
        
        # get conditional probability table for this configuraion
        L <- as.list(c(0, config))
        L[1] <- T
        probs <- do.call("[", c(list(ptable), L))
        
        # for the rows that are in this configuration, sample
        whichRows <- apply(local, 1, function(x) all(x == config))
        
        dat[whichRows, i] <- creator(numberOfLevels,
                                        size    = sum(whichRows),
                                        replace = T,
                                        prob    = probs)
      }
      
      # old slow version
      # would be better to loop over the distinct values, rather than rows
      #for (j in 1:N){
      #  config <- local[j, ]
      #  L <- as.list(c(0, config))
      #  L[1] <- T
      #  probs <- do.call("[", c(list(ptable), L))
      #  dat[j, i] <- sample.int(numberOfLevels,
      #                          size    = 1,
      #                          replace = T,
      #                          prob    = probs)
      #}
    }
  }
  
  data.frame(lapply(dat, as.factor))
}

#' @export
marginal.probs <- function(bn, ptables){
  stopifnot(
    "bn" %in% class(bn),
    class(ptables) == "list",
    all(sapply(ptables, class) == "table")
  )
  
  nNodes <- length(bn)
  numberOfParents <- sapply(bn, length)
  
  # topologically sort
  nodeOrder <- topologicallyOrder(bn)
  
  numberOfLevelsAll <- lapply(seq_len(nNodes), function(i){
    dim(ptables[[i]])[1]
  })
  
  # loop through the nodes in order
  for (i in nodeOrder){
    ptable <- ptables[[i]]
    
    # first dimension is the values
    numberOfLevels <- numberOfLevelsAll[[i]]
    
    if (numberOfParents[i] == 0){
      dat[, i] <- creator(numberOfLevels,
                             size    = N,
                             replace = T,
                             prob    = ptable)
    }
    else {
      parents <- bn[[i]]
      
      local <- dat[, parents, drop = F]
      local <- data.frame(lapply(seq_len(ncol(local)), function(i){
        ls <- seq_len(numberOfLevelsAll[[parents[i]]])
        factor(local[, i], levels = ls)
      }))
      
      # find all the configurations of the parents that exist
      tab <- table(local)
      whichTab <- which(tab > 0, arr.ind = T)
      
      # loop over the configurations
      for (rowNum in seq.int(nrow(whichTab))){
        config <- whichTab[rowNum, ]
        
        # get conditional probability table for this configuraion
        L <- as.list(c(0, config))
        L[1] <- T
        probs <- do.call("[", c(list(ptable), L))
        
        # for the rows that are in this configuration, sample
        whichRows <- apply(local, 1, function(x) all(x == config))
        
        dat[whichRows, i] <- creator(numberOfLevels,
                                        size    = sum(whichRows),
                                        replace = T,
                                        prob    = probs)
      }
    }
  }
}

cptBinary <- function(i,j){
  as.table(array(c(i, j), 2))
}
