# Part of the "parental" package, http://github.com/rbtgde/parental
# 
# This software is distributed under the GPL-3 license.  It is free,
# open source, and has the attribution requirements (GPL Section 7) in
#   http://github.com/rbtgde/parental
# 
# Note that it is required that attributions are retained with each function.
#
# Copyright 2008 Robert J. B. Goudie, University of Warwick

context("Spaces")

test_that("enumerateBNSpace", {
  expect_that(length(enumerateBNSpace(1)), is_identical_to(1L))
  expect_that(length(enumerateBNSpace(2)), is_identical_to(3L))
  expect_that(length(enumerateBNSpace(3)), is_identical_to(25L))
})

test_that("enumerateBVSSpace", {
  expect_equal(length(enumerateBVSSpace(2, response = 2)), 2)
  expect_equal(length(enumerateBVSSpace(3, response = 1)), 4)
  expect_equal(length(enumerateBVSSpace(4, response = 1)), 8)
  
  # check everything is sorted
  expect_equal(sum(sapply(enumerateBVSSpace(4, response = 1), function(x){
    sapply(x, is.unsorted)
  })), 0)
})

test_that("enumerateParents", {
  expect_equal(length(enumerateParents(c(1, 2))), 4)
  expect_equal(length(enumerateParents(c(3, 2, 1))), 8)
  expect_equal(length(enumerateParents(c(4, 2, 3, 1))), 16)
  
  
  expect_equal(sum(sapply(enumerateParents(c(12, 4, 5)), function(x){
    identical(x, integer(0))
  })), 1)
  
  expect_equal(sum(sapply(enumerateParents(c(12L, 4L, 5L)), function(x){
    identical(x, 12L)
  })), 1)
  
  expect_equal(sum(sapply(enumerateParents(c(12L, 4L, 5L)), function(x){
    identical(x, c(5L, 12L))
  })), 1)
  
  expect_equal(sum(sapply(enumerateParents(c(12L, 4L, 5L)), function(x){
    identical(x, c(4L, 5L, 12L))
  })), 1)
  
  # using maxNumberParents
  # this should not be in the set
  expect_equal(sum(sapply(enumerateParents(c(12L, 4L, 5L), max = 2), function(x){
    identical(x, c(4L, 5L, 12L))
  })), 0)
  
  expect_equal(length(enumerateParents(c(12L, 4L, 5L), max = 2)), 7)
})


test_that("combn2", {
  expect_identical(combn2(integer(0), 0L), list())
  
  expect_identical(combn2(1L, 1L), list(1L))
  expect_identical(combn2(2L, 1L), list(2L))
  
  expect_identical(combn2(c(1L, 2L), 1L), list(1L, 2L))
  expect_identical(combn2(c(1L, 2L), 2L), list(c(1L, 2L)))

  # sorting
  expect_identical(combn2(c(2L, 1L), 2L), list(c(1L, 2L)))
  expect_identical(combn2(c(1L, 3L, 2L), 3L), list(c(1L, 2L, 3L)))
  
  expected <- list(c(1L, 2L),
                   c(1L, 3L),
                   c(2L, 3L))
  expect_identical(combn2(c(1L, 3L, 2L), 2L), expected)
})

test_that("combn2 required=integer(0)", {
  expect_identical(combn2(integer(0), 0L, integer(0)), list())
  
  expect_identical(combn2(1L, 1L, integer(0)), list(1L))
  expect_identical(combn2(2L, 1L, integer(0)), list(2L))
  
  expect_identical(combn2(c(1L, 2L), 1L, integer(0)), list(1L, 2L))
  expect_identical(combn2(c(1L, 2L), 2L, integer(0)), list(c(1L, 2L)))

  # sorting
  expect_identical(combn2(c(2L, 1L), 2L, integer(0)), list(c(1L, 2L)))
  expect_identical(combn2(c(1L, 3L, 2L), 3L, integer(0)), list(c(1L, 2L, 3L)))
  
  expected <- list(c(1L, 2L),
                   c(1L, 3L),
                   c(2L, 3L))
  expect_identical(combn2(c(1L, 3L, 2L), 2L, integer(0)), expected)
})

test_that("combn2 required=3L", {
  expect_identical(combn2(integer(0), 0L, 3L), list(3L))
  
  expect_identical(combn2(1L, 1L, 3L), list(c(1L, 3L)))
  expect_identical(combn2(2L, 1L, 3L), list(c(2L, 3L)))
  
  expect_identical(combn2(c(1L, 2L), 1L, 3L), list(c(1L, 3L), c(2L, 3L)))
  expect_identical(combn2(c(1L, 2L), 2L, 3L), list(c(1L, 2L, 3L)))

  # sorting
  expect_identical(combn2(c(2L, 1L), 2L, 3L), list(c(1L, 2L, 3L)))
  expect_identical(combn2(c(1L, 3L, 2L), 3L, 3L), list(c(1L, 2L, 3L, 3L)))
  
  expected <- list(c(1L, 2L, 3L),
                   c(1L, 3L, 3L),
                   c(2L, 3L, 3L))
  expect_identical(combn2(c(1L, 3L, 2L), 2L, 3L), expected)
})

test_that("combn3", {
  expect_identical(combn3(integer(0), 0L), list())
  
  expect_identical(combn3(1L, 1L), list(1L))
  expect_identical(combn3(2L, 1L), list(2L))
  
  expect_identical(combn3(c(1L, 2L), 1L), list(1L, 2L))
  expect_identical(combn3(c(1L, 2L), 2L), list(c(1L, 2L)))

  # sorting
  expect_identical(combn3(c(2L, 1L), 2L), list(c(1L, 2L)))
  expect_identical(combn3(c(1L, 3L, 2L), 3L), list(c(1L, 2L, 3L)))
  
  expected <- list(c(1L, 2L),
                   c(1L, 3L),
                   c(2L, 3L))
  expect_identical(combn3(c(1L, 3L, 2L), 2L), expected)
})


test_that("combn3 required=integer(0)", {
  expect_identical(combn3(integer(0), 0L, integer(0)), list())
  
  expect_identical(combn3(1L, 1L, integer(0)), list(1L))
  expect_identical(combn3(2L, 1L, integer(0)), list(2L))
  
  expect_identical(combn3(c(1L, 2L), 1L, integer(0)), list(1L, 2L))
  expect_identical(combn3(c(1L, 2L), 2L, integer(0)), list(c(1L, 2L)))

  # sorting
  expect_identical(combn3(c(2L, 1L), 2L, integer(0)), list(c(1L, 2L)))
  expect_identical(combn3(c(1L, 3L, 2L), 3L, integer(0)), list(c(1L, 2L, 3L)))
  
  expected <- list(c(1L, 2L),
                   c(1L, 3L),
                   c(2L, 3L))
  expect_identical(combn3(c(1L, 3L, 2L), 2L, integer(0)), expected)
})

test_that("combn3 required=integer(0)", {
  expect_identical(combn3(integer(0), 0L, 3L), list(3L))
  
  expect_identical(combn3(1L, 1L, 3L), list(c(1L, 3L)))
  expect_identical(combn3(2L, 1L, 3L), list(c(2L, 3L)))
  
  expect_identical(combn3(c(1L, 2L), 1L, 3L), list(c(1L, 3L), c(2L, 3L)))
  expect_identical(combn3(c(1L, 2L), 2L, 3L), list(c(1L, 2L, 3L)))

  # sorting
  expect_identical(combn3(c(2L, 1L), 2L, 3L), list(c(1L, 2L, 3L)))
  expect_identical(combn3(c(1L, 3L, 2L), 3L, 3L), list(c(1L, 2L, 3L, 3L)))
  
  expected <- list(c(1L, 2L, 3L),
                   c(1L, 3L, 3L),
                   c(2L, 3L, 3L))
  expect_identical(combn3(c(1L, 3L, 2L), 2L, 3L), expected)
})

