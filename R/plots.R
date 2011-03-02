

#' Barchart of a graph
#'
#' Plot a barchart of summary statistics about a 'parental' object.
#'
#' @param x An object of class parental
#' @param data currently unused
#' @param type A character vector of length one. One of \code{indegree}, 
#'   \code{outdegree}
#' @param ... Further arguments, passed onto \code{barchart()}
#' @return A barchart
#' @S3method barchart parental
#' @method barchart parental
barchart.parental <- function(x,
                              data,
                              type = c("indegrees", "outdegrees"),
                              ...){
  
  nodeNames <- names(x)
  if (is.null(nodeNames)){
    nodeNames <- as.character(seq_along(x))
  }
  
  statistics <- list()
  index <- 1
  
  # all these statistics MUST return a vector 
  # of length nNodes(x)
  if ("indegrees" %in% type){
    statistic <- indegrees(x)
    names(statistic) <- nodeNames
    statistics[[index]] <- statistic
    names(statistics)[index] <- "Indegrees"
    index <- index + 1
  }
  if ("outdegrees" %in% type){
    statistic <- indegrees(x)
    names(statistic) <- nodeNames
    statistics[[index]] <- statistic
    names(statistics)[index] <- "Outdegrees"
    index <- index + 1
  }
  
  df <- cbind(names = nodeNames,
              do.call("make.groups", statistics))
  
  barchart(data ~ names | which,
           data = df,
           xlab = "Node",
           ylab = "Statistic",
           ...)
}