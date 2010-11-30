
if (as.numeric(R.version$minor) < 11){
  vapply <- function(X, FUN, FUN.VALUE, ..., USE.NAMES = TRUE){
  sapply(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE)
  }
}

is_within <- function(expected, tolerance){
  name <- deparse(substitute(expected))
  function(actual) {
      testthat:::expectation(
        identical(all.equal.numeric(expected, actual, tolerance = tolerance, scale = 1), TRUE),
        paste("was out by ", abs(expected - actual), " when max is ", tolerance, sep = "")
      )
    }
}