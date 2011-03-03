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
    possibleParents <- list(integer(0))
  }
  else {
    allowedNumberOfParents <- min(maxNumberParents, length(potentialParents))
    allowedNumberOfParentsSeq <- seq_len(allowedNumberOfParents)
  
    # all combinations of all lengths
    # unlisting is unelegant, but does the job
    possibleParents <- unlist(lapply(allowedNumberOfParentsSeq, 
      function(numberOfParents){
        combn3(potentialParents, numberOfParents, required)
      }
      ), 
    recursive = F)
    possibleParents <- c(possibleParents, list(integer(0)))
  }
  
  possibleParents
}


#' (Fast, simple) Generate All Combinations of n Elements, Taken m at a Time
#' 
#' A fast, simple version of \code{\link[utils]{combn}}.
#'
#' ALSO NOTE. The output is SORTED!
#'
#' If \code{x} is of length 0, then a blank list \code{list()} is returned,
#' unless \code{required} is of length great than 0, in which case,
#' list(required) is returned.
#'
#' @param x The set of elements from which to choose. Unlike \code{combn}, 
#'   if this is a single integer, only this single integer is used; 
#'   \code{combn} uses \code{1:x} in this case.
#' @param m The size of the sets
#' @param required A numeric vector that is appended to each set
#' @return A list of the combinations
#' @export
combn2 <- function (x, m, required = integer(0)) 
{
    stopifnot(length(m) == 1L)
    x <- sort.int(x)
    class(x) <- "integer"
    if (m == 0){
      if (length(required) > 0){
        return(list(required))
      } else {
        return(list())
      }
    }
    n <- length(x)
    if (n < m) 
        stop("n < m")
    m <- as.integer(m)
    e <- 0
    h <- m
    a <- 1L:m
    r <- x[a]
    
    count <- as.integer(round(choose(n, m)))
    out <- vector("list", count)
    out[[1L]] <- sort.int(c(r, required))
    
    i <- 2L
    nmmp1 <- n - m + 1L
    while (a[1L] != nmmp1) {
        if (e < n - h) {
            h <- 1L
            e <- a[m]
            j <- 1L
        }
        else {
            e <- a[m - h]
            h <- h + 1L
            j <- 1L:h
        }
        a[m - h + j] <- e + j
        r <- sort.int(c(x[a], required))
        class(r) <- "integer"
        out[[i]] <- r
        i <- i + 1L
    }
    out
}

#' (C version) Generate All Combinations of n Elements, Taken m at a Time
#' 
#' A fast, simple version of \code{\link[utils]{combn}}.
#' 
#' ALSO NOTE. The output is SORTED!
#'
#' If \code{x} is of length 0, then a blank list \code{list()} is returned,
#' unless \code{required} is of length great than 0, in which case,
#' list(required) is returned.
#' 
#' @param x The set of elements from which to choose. Unlike \code{combn}, 
#'   if this is a single integer, only this single integer is used; 
#'   \code{combn} uses \code{1:x} in this case. (This is also unlike the
#'   version in \code{gRbase}.
#' @param m The size of the sets
#' @param required A numeric vector that is appended to each set.
#'   The length of required must be positive!
#' @return A list of the combinations
#' @export
combn3 <- function(x, m, required = integer(0)){
  # From
  # http://cran.r-project.org/web/packages/gRbase/
  # Soren Hojsgaard and Claus Dethlefsen with contributions from Clive Bowsher
  # GPL License
  if (length(x) == 0){
    if (length(required) > 0){
      return(list(required))
    } else {
      return(list())
    }
  }
  if (length(x) < m){
    stop("Error in combnPrim: n < m\n")
  }
  x <- sort.int(x, method = "quick")
  NCAND = as.integer(length(x))
  NSEL = as.integer(m)
  NSET <- as.integer(choose(NCAND, NSEL))
  ANS <- as.integer(rep.int(0, NSET * NSEL))
  res <- .C("combnC", NSEL, NCAND, NSET, ANS, DUP = FALSE, 
      PACKAGE = "parental")[[4]]
  res <- x[res]
  dim(res) <- c(NSEL, NSET)
  if (!missing(required) && length(required) > 0){
    res <- rbind(res, rep(required, ncol(res)))
    res <- apply(res, 2, sort.int, method = "quick")
  }
  as.list(as.data.frame(res, optional = T, row.names = NULL))
}
