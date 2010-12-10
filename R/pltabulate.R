#' Tabulate a parental list
#'
#' Tabulate a parental list.
#'
#' @param pl A \code{parental.list} to tabulate.
#' @param pretty A logical indicating how to convert the objects of
#'   class \code{parental} inside \code{pl} to strings. See 
#'   \code{\link{as.character.parental.list}} for details.
#' @param levels What levels to make the table using. By default 
#'   \code{as.character(pl)}.
#' @param sort A logical indicating whether to sort the table.
#' @export
pltabulate <- function(pl, pretty = F, levels = NULL, sort = F){
  stopifnot(
    "parental.list" %in% class(pl)
  )
  # the following appears to use more memory
  # sort(table(factor(do.call("paste", list(pl, sep = ",")))))
  
  if (pretty){
    pretty <- as.character(pl, pretty = pretty)
    if (!is.null(levels)){
      res <- table(factor(pretty, levels = levels), dnn = NULL)
    }
    else {
      res <- table(pretty, dnn = NULL)
    }
  }
  else {
    if (!is.null(levels)){
      # Previously: 
      # table(factor(unlist(lapply(pl, as.character),
      #       use.names = F, recursive = F), levels = levels))
      res <- table(factor(as.character(pl), levels = levels), dnn = NULL)
    }
    else {
      # Previously:
      # sort(table(factor(unlist(lapply(pl, as.character),
      #      use.names = F, recursive = F))))
      res <- table(as.character(pl), dnn = NULL)
    }
  }
  
  if (sort){
    as.table(sort.int(res, method = "shell"))
  }
  else {
    res
  }
}