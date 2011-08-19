# Part of the "parental" package, http://github.com/rjbgoudie/parental
# 
# This software is distributed under the GPL-3 license.  It is free,
# open source, and has the attribution requirements (GPL Section 7) in
#   http://github.com/rjbgoudie/parental
# 
# Note that it is required that attributions are retained with each function.
#
# Copyright 2008 Robert J. B. Goudie, University of Warwick

context("Spaces")

test_that("enumerateBNSpace", {
  bn1 <- enumerateBNSpace(1)
  bn2 <- enumerateBNSpace(2)
  bn3 <- enumerateBNSpace(3)
  
  expect_that(length(bn1), is_identical_to(1L))
  expect_that(length(bn2), is_identical_to(3L))
  expect_that(length(bn3), is_identical_to(25L))
  
  bn1expect <- bn.list(bn(integer(0)))
  expect_that(bn1, is_identical_to(bn1expect))
  
  bn2expect <- bn.list(bn(integer(0), integer(0)),
                       bn(2, integer(0)),
                       bn(integer(0), 1))
  expect_that(bn2, is_identical_to(bn2expect))
  
  bn3expect <- bn.list(
    bn(integer(0), integer(0), integer(0)),
    bn(integer(0), integer(0),          1),
    bn(integer(0), integer(0),          2), # 3
    bn(integer(0),          1, integer(0)),
    bn(integer(0),          3, integer(0)),
    bn(         2, integer(0), integer(0)), # 6
    bn(         3, integer(0), integer(0)),
    bn(integer(0),          1,          1),
    bn(integer(0),          1,          2), # 9
    bn(integer(0),          3,          1),
    bn(         2, integer(0),          2),
    bn(         2, integer(0),          1), # 12
    bn(         3, integer(0),          2),
    bn(         3,          3, integer(0)),
    bn(         3,          1, integer(0)), # 15
    bn(         2,          3, integer(0)),
    bn(integer(0),    c(1, 3), integer(0)),
    bn(integer(0), integer(0),    c(1, 2)), # 18
    bn(   c(2, 3), integer(0), integer(0)),
    bn(         3,    c(1, 3), integer(0)),
    bn(integer(0),    c(1, 3),          1), # 21
    bn(         2, integer(0),    c(1, 2)),
    bn(integer(0),          1,    c(1, 2)),
    bn(   c(2, 3),          3, integer(0)), # 24
    bn(   c(2, 3), integer(0),          2)
    )
    
  len.ok <- length(bn3) == length(bn3expect)
  is.ok <- all(sapply(bn3, function(net){
    any(sapply(bn3expect, identical, net))
  }))
  expect_true(is.ok & len.ok)
})

test_that("enumerateBNSpace - banned", {
  bn2 <- enumerateBNSpace(2, banned = list(integer(0), 1))
  bn3 <- enumerateBNSpace(3, banned = list(integer(0), 1, 2))
  
  bn2expect <- bn.list(bn(integer(0), integer(0)),
                       bn(2, integer(0)))
  expect_that(bn2, is_identical_to(bn2expect))
  
  bn3expect <- bn.list(
    bn(integer(0), integer(0), integer(0)),
    bn(integer(0), integer(0),          1),
    bn(integer(0),          3, integer(0)),
    bn(         2, integer(0), integer(0)), # 6
    bn(         3, integer(0), integer(0)),
    bn(integer(0),          3,          1),
    bn(         2, integer(0),          1), # 12
    bn(         3,          3, integer(0)),
    bn(         2,          3, integer(0)),
    bn(   c(2, 3), integer(0), integer(0)),
    bn(   c(2, 3),          3, integer(0))
    )
    
  len.ok <- length(bn3) == length(bn3expect)
  is.ok <- all(sapply(bn3, function(net){
    any(sapply(bn3expect, identical, net))
  }))
  expect_true(is.ok & len.ok)
})

test_that("enumerateBNSpace - banned 2", {
  bn3 <- enumerateBNSpace(3, banned = list(integer(0), integer(0), c(1, 2)))
  
  bn3expect <- bn.list(
    bn(integer(0), integer(0), integer(0)),
    bn(integer(0),          1, integer(0)),
    bn(integer(0),          3, integer(0)),
    bn(         2, integer(0), integer(0)), # 6
    bn(         3, integer(0), integer(0)),
    bn(         3,          3, integer(0)),
    bn(         3,          1, integer(0)), # 15
    bn(         2,          3, integer(0)),
    bn(integer(0),    c(1, 3), integer(0)),
    bn(   c(2, 3), integer(0), integer(0)),
    bn(         3,    c(1, 3), integer(0)),
    bn(   c(2, 3),          3, integer(0))
    )
    
  len.ok <- length(bn3) == length(bn3expect)
  is.ok <- all(sapply(bn3, function(net){
    any(sapply(bn3expect, identical, net))
  }))
  expect_true(is.ok & len.ok)
})

test_that("enumerateBNSpace - required 1", {
  bn3 <- enumerateBNSpace(3, required = list(integer(0), integer(0), c(1, 2)))

  bn3expect <- bn.list(
    bn(integer(0), integer(0),    c(1, 2)),
    bn(         2, integer(0),    c(1, 2)),
    bn(integer(0),          1,    c(1, 2)))
    
    len.ok <- length(bn3) == length(bn3expect)
    is.ok <- all(sapply(bn3, function(net){
      any(sapply(bn3expect, identical, net))
    }))
    expect_true(is.ok & len.ok)
})

test_that("enumerateBNSpace - required 2", {
  bn3 <- enumerateBNSpace(3, required = list(integer(0), 1, integer(0)))

  bn3expect <- bn.list(
    bn(integer(0),          1, integer(0)),
    bn(integer(0),          1,          1),
    bn(integer(0),          1,          2), # 9
    bn(         3,          1, integer(0)), # 15
    bn(integer(0),    c(1, 3), integer(0)),
    bn(         3,    c(1, 3), integer(0)),
    bn(integer(0),    c(1, 3),          1), # 21
    bn(integer(0),          1,    c(1, 2))
    )
  
  len.ok <- length(bn3) == length(bn3expect)
  is.ok <- all(sapply(bn3, function(net){
    any(sapply(bn3expect, identical, net))
  }))
  expect_true(is.ok & len.ok)
})

test_that("enumerateBNSpace - maxIndegree", {
  expect_error(enumerateBNSpace(2, maxIndegree = -1))
  expect_error(enumerateBNSpace(2, maxIndegree = 3))
  expect_error(enumerateBNSpace(2, maxIndegree = 1.5))
  expect_error(enumerateBNSpace(2, maxIndegree = c(1, 2)))

  expect_equal(length(enumerateBNSpace(2, maxIndegree = 0)), 1)
  expect_equal(length(enumerateBNSpace(100, maxIndegree = 0)), 1)

  expected <- bn.list(bn(c(), c()),
                      bn(c(2), c()),
                      bn(c(), c(1)))
  expect_equal(length(enumerateBNSpace(2, maxIndegree = 1)), 3)
  expect_equal(enumerateBNSpace(2, maxIndegree = 1), expected)

  getMaxIndegree <- function(x, maxIndegree){
    max(sapply(x, length)) <= maxIndegree
  }

  all <- enumerateBNSpace(3)

  getMaxIndegree2 <- function(x) getMaxIndegree(x, 2)
  filtered <- Filter(getMaxIndegree2, all)
  expect_identical(enumerateBNSpace(3, maxIndegree = 2), filtered)

  getMaxIndegree1 <- function(x) getMaxIndegree(x, 1)
  filtered <- Filter(getMaxIndegree1, all)
  expect_identical(enumerateBNSpace(3, maxIndegree = 1), filtered)
})

test_that("enumerateBNSpace - maxIndegree and required", {
  enumerateBNSpace(3, maxIndegree = 1, required = list(c(), c(), c(1)))
  
  enumerateBNSpace(3, maxIndegree = 1, required = list(c(), c(), c(1, 2)))
})

# test_that("enumerateBNSpace - required 2", {
#   banned <- list(c(2,3,4), c(1,3,4), integer(0), 3)
#   required <- list(integer(0), integer(0), 4, integer(0))
#   bn3 <- enumerateBNSpace(4,
#                       banned = banned,
#                       required = required)
# 
#   bn3expect <- bn.list(
#     bn(integer(0),          1, integer(0)),
#     bn(integer(0),          1,          1),
#     bn(integer(0),          1,          2), # 9
#     bn(         3,          1, integer(0)), # 15
#     bn(integer(0),    c(1, 3), integer(0)),
#     bn(         3,    c(1, 3), integer(0)),
#     bn(integer(0),    c(1, 3),          1), # 21
#     bn(integer(0),          1,    c(1, 2))
#     )
#   
#   len.ok <- length(bn3) == length(bn3expect)
#   is.ok <- all(sapply(bn3, function(net){
#     any(sapply(bn3expect, identical, net))
#   }))
#   expect_true(is.ok & len.ok)
# })



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

