# Part of the "parental" package, http://github.com/rbtgde/parental
# 
# This software is distributed under the GPL-3 license.  It is free,
# open source, and has the attribution requirements (GPL Section 7) in
#   http://github.com/rbtgde/parental
# 
# Note that it is required that attributions are retained with each function.
#
# Copyright 2008 Robert J. B. Goudie, University of Warwick

#' Tests for whole numbers
#' 
#' Tests if x is a whole number, up to the tolerance specified by tol.
#' Taken from an example on the help page for \code{\link{is.integer}}. 
#' 
#' Note that is.integer(x) does not test if x contains integer numbers! 
#' 
#' @param x A numeric vector of length 1, which the function check if it is 
#'   whole
#' @param tol A numeric vector of length 1 specifying the tolerance allowed
#' 
#' @return A logical vector of length 1, taking the value TRUE is x is a 
#'   whole number and false otherwise
#' @export
#' @seealso \code{\link{is.integer}}
#' @examples
#' is.wholenumber(2)
#' is.wholenumber(2.1)
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5){
  stopifnot(length(x) == 1,
            isTRUE(is.numeric(x)),
            length(tol) == 1,
            isTRUE(is.numeric(tol)))
  abs(x - round(x)) < tol
}

#' Retrieve off-diagonal elements
#'
#' Retrieve off-diagonal elements
#'
#' @param mx A matrix
#' @return A vector of off-diagonals
#' @seealso \code{\link{diag}}, \code{\link{lower.tri}}, 
#'   \code{\link{upper.tri}}
#' @export
#' @examples
#' m <- matrix(1:9, 3, 3)
#' notdiag(m)
notdiag <- function(mx){
  mx[which(upper.tri(mx) + lower.tri(mx) == 1)]
}
