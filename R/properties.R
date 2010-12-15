#' Number of nodes
#' 
#' A generic to get the number of nodes.
#' 
#' @param x ...
#' @param ... ...
#' @return The number of nodes in \code{parental}, an integer.
#' @export
nNodes <- function(x, ...){
  UseMethod("nNodes")
}

#' Number of nodes/vertices
#' 
#' Get the number of nodes in a \code{parental} object
#' 
#' @param parental An object of class \code{parental}
#' @return The number of nodes in \code{parental}, an integer.
#' @export
#' @S3method nNodes parental
nNodes.parental <- function(parental){
  stopifnot(
    "parental" %in% class(parental)
  )
  length(parental)
}

#' Number of edges
#' 
#' Get the number of edges in a \code{parental} object
#' 
#' @param parental An object of class \code{parental}
#' @return The number of edges in \code{parental}, an integer.
#' @export
nEdges <- function(parental){
  stopifnot("parental" %in% class(parental))
  length(unlist(parental))
}

#' Children lists
#' 
#' Create a list, with each component listing the direct children of the 
#' corresponding node in the supplied parental.
#' 
#' @param parental An object of class \code{parental}
#' @return A list, the ith component of which is a numeric vector listing 
#'   the nodes that are children of node i.
#' @export
getChildren <- function(parental){
  stopifnot("parental" %in% class(parental))
  
  seq <- seq_along(parental)
  lapply(seq, function(i){
    unlist(
      sapply(seq, function(j){
        if (i %in% parental[[j]]){
          j
        }
        else {
          integer(0)
        }
      })
    )
  })
  
  #nodeSeq <- seq_along(parental)
  #nNodes <- length(parental)
  #out <- vector("list", length = nNodes)
  #
  ## for each node
  #lapply(nodeSeq, function(i){
  #  # get that node's parents
  #  parents <- parental[[i]]
  #  
  #  nParents <- length(parents)
  #  parentsSeq <- seq_len(nParents)
  #  
  #  # add the current node's number
  #  # to each of the parents children
  #  # lists
  #  new <- as.list(rep(i, nParents))
  #  out[parents] <<- lapply(parentsSeq, function(i){
  #    node <- parents[[i]]
  #    c(new[[i]], out[[node]])
  #  })
  #})
  #out
}

#' Acyclicity testing
#' 
#' Check for cycles in a directed graph.
#' 
#' @param parental An object of class \code{parental}
#' @return A logical. TRUE if \code{parental} is acyclic. FALSE if 
#'   \code{parental} contains a cycle.
#' @export
checkAcyclic <- function(parental){
  stopifnot("parental" %in% class(parental))
  # Graphs, Networks and Algorithms By Dieter Jungnickel
  # Third Edition p48

  N <- 1
  indegrees <- sapply(parental, length)
  L <- which(indegrees == 0)
  topnr <- numeric(length = nNodes(parental))
  children <- getChildren(parental)
  while (length(L) > 0){
    v <- L[1]
    L <- setdiff(L, v)
    topnr[v] <- N
    N <- N + 1
    for (w in children[[v]]){
      indegrees[w] <- indegrees[w] - 1
      if (indegrees[w] == 0){
        L <- union(L,  w)
      }
    }
  }
  
  # true if is acyclic
  # ie TRUE == no cycle
  # FALSE = cycle exists
  if (N == nNodes(parental) + 1) T else F
}

#' Topological ordering
#' 
#' Finds a permutation of the nodes 1, ..., p such that each all ancestors 
#' of each node have a lower place in the order.
#' 
#' @param parental An object of class \code{parental}
#' @return A numeric vector. The order.
#' @export
topologicallyOrder <- function(parental){
  stopifnot("parental" %in% class(parental))
  # slow but worth it for error checking for now
  if (!checkAcyclic(parental)) stop("The BN must be acyclic")
  
  N <- 1
  indegrees <- sapply(parental, length)
  L <- which(indegrees == 0)
  topnr <- numeric(length = nNodes(parental))
  
  order <- rep(NA, length(parental))
  order[seq_along(L)] <- L
  children <- getChildren(parental)
  wh <- vector("numeric", 1)
  
  while (length(L) > 0){
    v <- L[1]
    
    L <- setdiff(L, v)
    topnr[v] <- N
    N <- N + 1
    for (w in children[[v]]){
      indegrees[w] <- indegrees[w] - 1
      
      if (indegrees[w] == 0){
        L <- union(L,  w)
        wh <- match(T, is.na(order))
        order[wh] <- w
      }
    }
  }
  order
}

#' A dual-direction, fast (and dangerous!) version of setdiff.
#'
#' setdiff() is not symmetric. This function returns a list with component 
#' 1 equivalent to setdiff(x, y) and component 2 equivalent to 
#' setdiff(y, x).
#'
#' Note that unlike setdiff() THE OUTPUT MAY CONTAIN DUPLICATES. 
#' eg     setdiff2(c(1,1,2), c(2, 3))[[1]] == c(1, 1)
#'    BUT setdiff(c(1, 1, 2), c(2, 3)) == 1
#' 
#' Additionally the inputs are NOT coerced to vectors, again, unlike setdiff
#' 
#' @param x A numeric vector
#' @param y A numeric vector
#' @return A list of length 2, with setdiff(x, y) in component 1 and 
#'   setdiff(y, x) in component 2 (apart from the differences described 
#'   above).
#' @export
setdiff2 <- function(x, y){
  list(x[match(x, y, 0L) == 0L], y[match(y, x, 0L) == 0L])
}

#' Compute the number of edge additions, removals (and single-edge flips if 
#' allowFlips = T) that would be required to morph from network x to network 
#' y, both of which are objects of class 'parental'. The measure is 
#' symmetric.
#'
#' The parental objects x and y must have the same number of 
#' nodes. No attempt is made to account for whether intermediate graphs are 
#' cyclic (but perhaps there is always one ordering of the moves that is 
#' OK?).
#' 
#' 
#' @param x An object of class "parental"
#' @param y An object of class "parental"
#' @param components A logical of length 1, indicating whether the 
#'   total number of moves required to morph x to y should be returned 
#'   (components = F) or if the number of changes required for 
#'   each node should be returned (components = T). 
#' @param allowFlips Allow single-edge flip moves. This is not compatible 
#'   with components = T, because it is not clear for which node to 
#'   account flip moves.
#' @return if components == FALSE:
#'     A numeric of length 1 indicating the number of moves required.
#'   if components == TRUE:
#'     A number number of length (nNodes(x) == nNodes(y)).
#'     The figure in position i of the vector relates to the number changes 
#'     that need to be made to change the inward bound edges toward node i.
#' @export
numberOfMovesBetweenIgnoringCycles <- function(x, y, 
                                               components = F,
                                               allowFlips = F){
  stopifnot(
    length(x) == length(y),
    "parental" %in% class(x),
    "parental" %in% class(y),
    class(components) == "logical",
    length(components) == 1,
    class(allowFlips) == "logical",
    length(allowFlips) == 1
  )
  if (isTRUE(components) && isTRUE(allowFlips)){
    stop("components and allFlips cannot both be true")
  }
  
  diffs <- mapply(setdiff2, x, y, SIMPLIFY = F, USE.NAMES = F)
  if (allowFlips){
    # loop over each node
    for (currentchild in seq_along(x)){
      
      # for each difference
      toremove <- integer(0)
      sq <- seq_along(diffs[[currentchild]][[1]])
      for (wh in sq){
        currentparent <- diffs[[currentchild]][[1]][wh]
        
        if (currentchild %in% y[[currentparent]]){
          toremove <- c(toremove, wh)
        }
      }
      new <- setdiff(sq, toremove)
      diffs[[currentchild]][[1]] <- diffs[[currentchild]][[1]][new]
    }
  }
  
  if (components){
    getComponentLength <- function(x, i){
      length(x[[i]])
    }
    sapply(diffs, getComponentLength, 1) + 
      sapply(diffs, getComponentLength, 2)
  }
  else {
    length(unlist(diffs, use.names = F))
  }
}

#' Unknown
#' 
#' @param x An object of class \code{parental}
#' @param y An object of class \code{parental}
#' @export
route <- function(x, y){
  stopifnot(
    length(x) == length(y),
    "parental" %in% class(x),
    "parental" %in% class(y)
  )
  
  diffs <- mapply(setdiff2, x, y, SIMPLIFY = F, USE.NAMES = F)
  browser()
}

#' ...
#' 
#' ....
#' @param parental1 ...
#' @param parental2 ...
#' @param count ...
#' @export
psetdiff <- function(parental1, parental2, count = F){
  stopifnot(
    length(parental1) == length(parental2),
    "parental" %in% class(parental1),
    "parental" %in% class(parental2),
    class(count) == "logical"
  )
  res <- mapply(setdiff, parental1, parental2,
                SIMPLIFY = F, USE.NAMES = F)
  if (count){
    length(unlist(res))
  }
  else {
    class(res) <- "parental"
    res
  }
}

#' ...
#' 
#' ....
#' @param parental1 ...
#' @param parental2 ...
#' @param count ...
#' @export
pintersect <- function(parental1, parental2, count = F){
  stopifnot(
    length(parental1) == length(parental2),
    "parental" %in% class(parental1),
    "parental" %in% class(parental2),
    class(count) == "logical"
  )
  
  res <- mapply(intersect, parental1, parental2)
  if (count){
    length(unlist(res))
  }
  else {
    class(res) <- "parental"
    res
  }
}

#' ...
#' 
#' ....
#' @param parental1 ...
#' @param parental2 ...
#' @export
punion <- function(parental1, parental2){
  stopifnot(
    length(parental1) == length(parental2),
    "parental" %in% class(parental1),
    "parental" %in% class(parental2)
  )
  
  res <- mapply(c, parental1, parental2)
  res <- lapply(res, function(parents){
    sort.int(unique(parents))
  })
  class(res) <- "parental"
  res
}

#' ...
#' 
#' ....
#' @param pl ...
#' @export
lpunion <- function(pl){
  stopifnot(
    "parental.list" %in% class(pl)
  )
  numberOfNodesVector <- sapply(pl, length)
  numberOfNodes <- numberOfNodesVector
  classVector <- sapply(pl, class)
  stopifnot(
    all(numberOfNodesVector[[1]], numberOfNodesVector)
  )
  numberOfParentals <- length(pl)
  
  out <- lapply(seq_len(numberOfNodes), function(i){
    select <- seq(i, numberOfNodes * numberOfParentals, by = numberOfNodes)
    sort.int(unique(unlist(unlist(pl, rec = F)[select])))
  })
  
  if (length(dim(classVector)) == 2){
    class(out) <- classVector[, 1]
  }
  else {
    class(out) <- classVector[1]
  }
  out
}

#' Returns a matrix encoding the number of routes between the nodes of the 
#' bn x.
#' 
#' Element (i, j) contains the number of routes from node i to node j
#' for i != j
#' Element (i, i) contains 1 for all i.
#' 
#' @param x An object of class 'bn'.
#' @return A matrix of dimension nNodes(x) x nNodes(x)
#' @export
routes <- function(x){
  stopifnot("bn" %in% class(x))
  nNodes <- nNodes(x)
  nodesSeq <- seq.int(nNodes)
  routes <- matrix(0, nNodes, nNodes)
  diag(routes) <- 1
  
  for (head in nodesSeq){
    for (tail in x[[head]]){
      routes <- routes + outer(routes[, tail], routes[head, ])
    }
  }
  routes
}

#' Update a routes matrix (edge addition)
#' 
#' A routes matrix is a matrix A, such that each element (i, j) is the 
#' number of routes from i to j in some directed graph.
#'
#' This function updates the routes matrix to account for the addition of 
#' an edge from i to j in the directed graph
#' 
#' @param x A routes matrix
#' @param i The node from which the added edge emanates
#' @param j The node that the added edge goes to
#' @export
routesAddEdge <- function(x, i, j){
  x + x[, i] %*% .Internal(t.default((x[j, ])))
}

#' Update a routes matrix (edge removal)
#' 
#' A routes matrix is a matrix A, such that each element (i, j) is the 
#' number of routes from i to j in some directed graph.
#'
#' This function updates the routes matrix to account for the deletion of 
#' an edge from i to j in the directed graph
#' 
#' @param x A routes matrix
#' @param i The node from which the removed edge emanates
#' @param j The node that the removed edge goes to
#' @export
routesRemoveEdge <- function(x, i, j){
  x - x[, i] %*% .Internal(t.default((x[j, ])))
}

#' Neighbourhood size
#' 
#' Computes the number of 
#' 
#' @param x An object of class \code{parental}
#' @return The size of the neighbourhood (an integer)
#' @export
neighbourhoodSize <- function(x){
  # this algorithm is pretty rubbish
  stopifnot("bn" %in% class(x))
  nNodes <- nNodes(x)
  nodesSeq <- seq.int(nNodes)
  routes <- matrix(0, nNodes, nNodes)
  diag(routes) <- 1
  adj <- matrix(0, nNodes, nNodes)

  for (head in nodesSeq){
    for (tail in x[[head]]){
      routes <- routes + outer(routes[, tail], routes[head, ])
      adj[tail, head] <- 1
    }
  }
  length(routes[routes == 0 | (routes == 1 & adj == 1)])
}
