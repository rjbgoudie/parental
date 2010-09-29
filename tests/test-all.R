library(testthat)
library(parental)

auto_test <- function (code_path, test_path, reporter = "summary") 
{
  reporter <- testthat:::find_reporter(reporter)
  testthat:::source_dir(code_path)
  testthat:::test_dir(test_path)
  starts_with <- function(string, prefix) {
    substr(string, 1, nchar(prefix)) == prefix
  }
  watcher <- function(added, deleted, modified) {
    changed <- c(added, modified)
    tests <- changed[starts_with(changed, test_path)]
    code <- changed[starts_with(changed, code_path)]
    if (length(code) > 0) {
      cat("Changed code: ", paste(basename(code), collapse = ", "), 
        "\n")
      cat("Rerunning all tests\n")
      lapply(code, source, chdir = TRUE)
      test_dir(test_path)
    }
    else if (length(tests) > 0) {
      cat("Rerunning tests: ", paste(basename(tests), collapse = ", "), 
        "\n")
      testthat:::with_reporter(reporter$clone(), lapply(tests, source, 
        chdir = TRUE))
    }
    TRUE
  }
  testthat:::watch(c(code_path, test_path), watcher, pattern = "[^test]")
}

# based on try() in base
tryWarning <- function (expr, silent = FALSE) 
{
  tryCatch(expr, warning = function(e) {
    call <- conditionCall(e)
    if (!is.null(call)) {
      if (identical(call[[1L]], quote(doTryCatch))) 
        call <- sys.call(-4L)
      dcall <- deparse(call)[1L]
      prefix <- paste("Error in", dcall, ": ")
      LONG <- 75L
      msg <- testthat:::conditionMessage(e)
      sm <- strsplit(msg, "\n")[[1L]]
      w <- 14L + nchar(dcall, type = "w") + nchar(sm[1L], 
        type = "w")
      if (is.na(w)) 
        w <- 14L + nchar(dcall, type = "b") + nchar(sm[1L], 
          type = "b")
      if (w > LONG) 
        prefix <- paste(prefix, "\n  ", sep = "")
    }
    else prefix <- "Warning : "
    msg <- paste(prefix, conditionMessage(e), "\n", sep = "")
    .Internal(testthat:::seterrmessage(msg[1L]))
    if (!silent && identical(getOption("show.error.messages"), 
      TRUE)) {
      cat(msg, file = stderr())
      .Internal(printDeferredWarnings())
    }
    invisible(structure(msg, class = "try-warning"))
  })
}

throws_warning <- function (regexp = NULL) 
{
  function(expr) {
    res <- tryWarning(force(expr), TRUE)
    if (!is.null(regexp)) {
      matches(regexp)(res)
    }
    else {
      testthat:::is_a("try-warning")(res)
    }
  }
}

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



test_package("parental")
