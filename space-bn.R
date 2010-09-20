# returns a list of every DAG with n nodes

enumerateBNSpace <- function(n, allowCyclic = FALSE, multicore = F){
  myapply <- function(...) lapply(...)
  if (multicore){
    require(multicore)
    myapply <- function(...) mclapply(..., mc.cores = 10)
  }
  
  combin <- function(element, n){
    unlist(
      lapply(seq_len(n-1), function(i){
        combn(
          setdiff(seq_len(n), element), # all elements except
                                        # the current element
          i, 
          simplify = F
        )
      }),
      recursive = F)
  }
  
  options <- lapply(seq_len(n), function(i) combin(i, n))
  
  # plus 1 for no parents
  choices <- expand.grid(lapply(options, function(option){
    seq_len(length(option) + 1)
  }))
  
  family <- myapply(seq_len(nrow(choices)), function(i){
    out <- unlist(
      lapply(seq_len(n), function(node){
        out <- options[[node]][choices[i,node]]
        if (is.null(out[[1]])){
          out[[1]] <- integer(0)
        }
        out
      }), 
      rec = F
    )
    class(out) <- c("bn", "parental")
    out
  })
  
  class(family) <- c("bn.list", "parental.list")
  
  if (allowCyclic){
    warning("** NOTE: ALLOWING CYCLIC GRAPHS **")
  }
  else {
    family <- filterCyclic(family)
  }
  
  family
}

filterCyclic <- function(bnlist){
  isAcyclic <- lapply(bnlist, checkAcyclic)
  out <- bnlist[which(isAcyclic == TRUE)]
  class(out) <- c("bn.list", "parental.list")
  out
}

bn.list <- function(...){
  out <- list(...)
  class(out) <- c("bn.list", "parental.list")
  out
}