# Part of the "parental" package, http://github.com/rbtgde/parental
# 
# This software is distributed under the GPL-3 license.  It is free,
# open source, and has the attribution requirements (GPL Section 7) in
#   http://github.com/rbtgde/parental
# 
# Note that it is required that attributions are retained with each function.
#
# Copyright 2008 Robert J. B. Goudie, University of Warwick

#' Undocumented.
#'
#' ...
#'
#' @param el ...
#' @param set ...
#' @return ...
#' @export
is.element2 <- function (el, set) {
  stopifnot("parental" %in% class(el),
            "parental.list" %in% class(set))
  any(sapply(set, function(x){
      identical(el, x)
  }))
}

#' Undocumented.
#'
#' ...
#'
#' @param el ...
#' @param set ...
#' @return ...
#' @export
which2 <- function (el, set) {
  stopifnot("parental" %in% class(el),
            "parental.list" %in% class(set))
  which(sapply(set, function(x){
      identical(el, x)
  }))
}