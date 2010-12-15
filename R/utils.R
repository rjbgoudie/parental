#' Tests if x is a whole number, up to the tolerance specified by tol.
#' Taken from an example on the help page for is.integer()
#' 
#' @param x A numeric vector of length 1, which the function check if it is 
#'   whole
#' @param tol A numeric vector of length 1 specifying the tolerance allowed
#' 
#' @return A logical vector of length 1, taking the value TRUE is x is a 
#'   whole number and false otherwise
#' @export
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
#' @export
notdiag <- function(mx){
  mx[which(upper.tri(mx) + lower.tri(mx) == 1)]
}
