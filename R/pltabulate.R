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