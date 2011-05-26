# Part of the "parental" package, http://github.com/rjbgoudie/parental
# 
# This software is distributed under the GPL-3 license.  It is free,
# open source, and has the attribution requirements (GPL Section 7) in
#   http://github.com/rjbgoudie/parental
# 
# Note that it is required that attributions are retained with each function.
#
# Copyright 2008 Robert J. B. Goudie, University of Warwick

context("Slow Spaces")

test_that("enumerateBNSpace", {
  expect_that(length(enumerateBNSpace(4)), is_identical_to(543L))
})

test_that("enumerateBNSpace - maxIndegree", {
  all <- enumerateBNSpace(4)

  getMaxIndegree <- function(x, maxIndegree){
    max(sapply(x, length)) <= maxIndegree
  }

  getMaxIndegree3 <- function(x) getMaxIndegree(x, 3)
  filtered <- Filter(getMaxIndegree3, all)
  expect_identical(enumerateBNSpace(4, maxIndegree = 3), filtered)

  getMaxIndegree2 <- function(x) getMaxIndegree(x, 2)
  filtered <- Filter(getMaxIndegree2, all)
  expect_identical(enumerateBNSpace(4, maxIndegree = 2), filtered)

  getMaxIndegree1 <- function(x) getMaxIndegree(x, 1)
  filtered <- Filter(getMaxIndegree1, all)
  expect_identical(enumerateBNSpace(4, maxIndegree = 1), filtered)
})
