#' ...
#'
#' ...
#' @export
is.element2 <- function (el, set) {
  stopifnot("parental" %in% class(el),
            "parental.list" %in% class(set))
  any(sapply(set, function(x){
      identical(el, x)
  }))
}

#' ...
#'
#' ...
#' @export
which2 <- function (el, set) {
  stopifnot("parental" %in% class(el),
            "parental.list" %in% class(set))
  which(sapply(set, function(x){
      identical(el, x)
  }))
}
