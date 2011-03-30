# Part of the "parental" package, http://github.com/rjbgoudie/parental
# 
# This software is distributed under the GPL-3 license.  It is free,
# open source, and has the attribution requirements (GPL Section 7) in
#   http://github.com/rjbgoudie/parental
# 
# Note that it is required that attributions are retained with each function.
#
# Copyright 2008 Robert J. B. Goudie, University of Warwick

context("Properties")

test_that("setdiff2", {
  x <- integer(0)
  y <- 1L
  out <- list(integer(0), 1L)
  expect_that(setdiff2(x, y), is_identical_to(out))
  
  x <- 1L
  y <- integer(0)
  out <- list(1L, integer(0))
  expect_that(setdiff2(x, y), is_identical_to(out))
  
  x <- c(1, 4, 0, 5)
  y <- c(8, 1, 4, 7)
  out <- list(c(0, 5), c(8, 7))
  expect_that(setdiff2(x, y), is_identical_to(out))
  
  set.seed(6041)
  x <- sample(10000, 500, replace = T)
  y <- sample(10000, 500, replace = T)
  out <- list(setdiff(x, y), setdiff(y, x))
  sd2 <- list()
  sd2[[1]] <- unique(setdiff2(x, y)[[1]])
  sd2[[2]] <- unique(setdiff2(x, y)[[2]])
  expect_that(sd2, is_identical_to(out))
})

test_that("numberOfMovesBetweenIgnoringCycles", {
  network1 <- bn(c(10L, 4L, 6L, 7L, 9L, 5L), c(8L, 10L, 6L, 4L, 5L, 9L, 7L, 1L, 3L), 7L, c(9L, 5L, 3L), 6L, c(3L, 7L), integer(0), c(5L, 3L, 7L, 1L, 10L, 9L, 6L), c(7L, 3L), c(3L, 4L, 7L, 5L))
  
  network2 <- bn(c(2L, 3L, 8L, 5L, 9L), integer(0), 9L, c(5L, 1L, 3L, 8L), 2L, 4L, c(5L, 6L, 4L, 2L, 8L, 3L), integer(0), c(2L, 5L, 8L), c(8L, 2L, 6L, 7L, 9L, 3L, 1L))
  
  network3 <- network1
  network3[[3]] <- integer(0)
  
  test0 <- numberOfMovesBetweenIgnoringCycles(network1, network1)
  expect_that(test0, is_identical_to(0L))
  test0 <- numberOfMovesBetweenIgnoringCycles(network2, network2)
  expect_that(test0, is_identical_to(0L))
  
  test1a <- numberOfMovesBetweenIgnoringCycles(network1, network2)
  test1b <- numberOfMovesBetweenIgnoringCycles(network2, network1)
  expect_that(test1a, is_identical_to(51L))
  expect_that(test1b, is_identical_to(51L))
  
  test2a <- numberOfMovesBetweenIgnoringCycles(network1, network3)
  test2b <- numberOfMovesBetweenIgnoringCycles(network3, network1)
  expect_that(test2a, is_identical_to(1L))
  expect_that(test2b, is_identical_to(1L))
  
  test3a <- numberOfMovesBetweenIgnoringCycles(network1, network2, 
                                              components = TRUE)
  test3b <- numberOfMovesBetweenIgnoringCycles(network2, network1, 
                                              components = TRUE)
  expect3 <- c(7L, 9L, 2L, 3L, 2L, 3L, 6L, 7L, 5L, 7L)
  expect_that(test3a, is_identical_to(expect3))
  expect_that(test3b, is_identical_to(expect3))
  
  test4 <- numberOfMovesBetweenIgnoringCycles(network1, network3,
                                              components = TRUE)
  expect4 <- c(0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L)
  expect_that(test4, is_identical_to(expect4))
  
  
  bn1 <- bn(2, integer(0))
  bn2 <- bn(integer(0), 1)
  
  test5 <- numberOfMovesBetweenIgnoringCycles(bn1, bn2)
  expect_that(test5, is_identical_to(2L))
  
  test6 <- numberOfMovesBetweenIgnoringCycles(bn1, bn2, components = TRUE)
  expect_that(test6, is_identical_to(c(1L, 1L)))
  
  test7 <- numberOfMovesBetweenIgnoringCycles(bn1, bn2, allowFlips = TRUE)
  expect_that(test7, is_identical_to(1L))
  
  expect_that(numberOfMovesBetweenIgnoringCycles(bn1, bn2,
                                                 allowFlips = TRUE,
                                                 components = TRUE),
                                                 throws_error())
  
  mode1 <- structure(list(19L, integer(0), c(12L, 13L, 24L), integer(0), 
    c(11L, 24L), 3:4, c(1L, 2L, 3L, 12L, 21L, 22L), 19L, c(1L, 
    4L, 5L, 19L), integer(0), 19:20, integer(0), integer(0), 
    2L, 23:24, c(4L, 9L), c(18L, 19L, 23L), integer(0), integer(0), 
    19L, 1L, c(2L, 3L, 17L, 18L), integer(0), integer(0), 5L), class = c("bn", 
    "parental"))
  
  mode2 <- structure(list(19L, integer(0), c(12L, 13L, 24L), integer(0), 
    c(11L, 24L), 3:4, c(1L, 2L, 3L, 12L, 21L, 22L), 19L, c(1L, 
    4L, 5L, 19L), integer(0), 19L, integer(0), integer(0), 2L, 
    23:24, c(4L, 9L), c(18L, 19L, 23L), integer(0), integer(0), 
    c(11L, 19L), 1L, c(2L, 3L, 17L, 18L), integer(0), integer(0), 
    5L), class = c("bn", "parental"))
    
    test9 <- numberOfMovesBetweenIgnoringCycles(mode1, mode2,
                                                allowFlips = TRUE)
  expect_that(test9, is_identical_to(1L))
  
  
  bn1 <- bn(3, c(1,3), integer(0))
  bn2 <- bn(2, integer(0), c(1, 2))
  
  test10 <- numberOfMovesBetweenIgnoringCycles(bn1, bn2)
  expect_that(test10, is_identical_to(6L))
  
  test11 <- numberOfMovesBetweenIgnoringCycles(bn1, bn2, allowFlips = T)
  expect_that(test11, is_identical_to(3L))
})


test_that("topologicallyOrder", {
  expect_that(topologicallyOrder(bn(integer(0))), is_identical_to(c(1L)))
  expect_that(topologicallyOrder(bn(integer(0), 1)), is_identical_to(c(1L, 2L)))
  expect_that(topologicallyOrder(bn(2, integer(0))), is_identical_to(c(2L, 1L)))
  
  expect_that(topologicallyOrder(bn(NULL)), is_identical_to(c(1L)))
  expect_that(topologicallyOrder(bn(NULL, 1)), is_identical_to(c(1L, 2L)))
  expect_that(topologicallyOrder(bn(2, NULL)), is_identical_to(c(2L, 1L)))
  
  expect_that(topologicallyOrder(bn(integer(0), 1, 2)), is_identical_to(c(1L, 2L, 3L)))
  expect_that(topologicallyOrder(bn(integer(0), 1, c(1, 2))), is_identical_to(c(1L,2L,3L)))
  
  expect_that(topologicallyOrder(bn(c(3,4),integer(0),c(4),c(2))), is_identical_to(c(2L, 4L, 3L, 1L)))
  expect_that(topologicallyOrder(bn(c(4L, 2L), integer(0), c(1L, 4L), integer(0))), is_identical_to(c(2L, 4L, 1L, 3L)))
  
  expect_that(topologicallyOrder(bn(c(5L, 2L), integer(0), c(1L, 4L, 5L), c(1L, 2L, 5L), integer(0))
), is_identical_to(c(2L, 5L, 1L, 4L, 3L)))
  
  expect_that(topologicallyOrder(bn(c(2L, 5L, 4L, 3L), 4L, 2L, integer(0), 3L, 4L)), is_identical_to(c(4L, 2L, 6L, 3L, 5L, 1L))) # c(4, 6, 2, 3, 5, 1) also valid
  
  expect_that(topologicallyOrder(bn(1, NULL)), throws_error())
})

test_that("checkAcyclic", {
  expect_that(checkAcyclic(bn(c(3,4),integer(0),c(4),c(2))), is_true())
  expect_that(checkAcyclic(bn(c(3,4),c(1),c(4),c(2))), is_false())
  expect_that(checkAcyclic(bn(c(3,4),c(1),integer(0),c(2))), is_false())
  expect_that(checkAcyclic(bn(c(3),c(1),integer(0),c(2))), is_true())
  expect_that(checkAcyclic(bn(integer(0),c(1))), is_true())
  expect_that(checkAcyclic(bn(2, 3, 1)), is_false())
  expect_that(checkAcyclic(bn(2, 3, 2)), is_false())
  expect_that(checkAcyclic(bn(2, 3, NULL)), is_true())
  expect_that(checkAcyclic(bn(1, NULL)), is_false())
})


test_that("getChildren", {
  expect_that(getChildren(bn(integer(0))), is_identical_to(list(integer(0))))
  expect_that(getChildren(bn(integer(0), 1)), is_identical_to(list(2L, integer(0))))
  expect_that(getChildren(bn(2, integer(0))), is_identical_to(list(integer(0), 1L)))
  
  expect_that(getChildren(bn(NULL)), is_identical_to(list(integer(0))))
  expect_that(getChildren(bn(NULL, 1)), is_identical_to(list(2L, integer(0))))
  expect_that(getChildren(bn(2, NULL)), is_identical_to(list(integer(0), 1L)))
  
  expect_that(getChildren(bn(integer(0), 1, 2)), is_identical_to(list(2L, 3L, integer(0))))
  expect_that(getChildren(bn(integer(0), 1, c(1, 2))), is_identical_to(list(c(2L, 3L), 3L, integer(0))))
  
  expect_that(getChildren(bn(c(3,4),integer(0),c(4),c(2))), is_identical_to(list(integer(0), 4L, 1L, c(1L, 3L))))
})


test_that("psetdiff", {
  
  net <- bn(c(7L, 9L), c(22L, 7L), c(22L, 7L), 9L, 24L, 3:4, integer(0), 
  c(19L, 3L), 5L, c(18L, 15L, 19L), 5L, c(7L, 3L), c(7L, 3L
  ), 2L, c(2L, 16L, 24L), c(3L, 9L, 4L), c(19L, 22L), c(17L, 
  22L), c(11L, 1L, 9L), c(19L, 11L, 18L), c(1L, 7L), 7L, c(15L, 
  17L, 9L), 3L, 5L)
  
  # dubious input, unsorted, but bn() handles this
  map <- bn(c(7, 9), c(7, 22), 24, 9, 11, c(3, 4), c(3, 22), c(3, 19
  ), 5, c(15, 18, 19), integer(0), c(3, 7), c(3, 7), 2, c(2, 16, 
  24), c(3, 4, 9), c(19, 22), c(17, 22), c(1, 9, 11), c(18, 19), 
  c(1, 7), 3, c(9, 15, 17), 5, 5)
  
  expect_that(psetdiff(net, map), is_identical_to(structure(list(integer(0), integer(0), c(7L, 22L), integer(0), 24L, integer(0), integer(0), integer(0), integer(0), integer(0), 5L, integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), 11L, integer(0), 7L, integer(0), 3L, integer(0)), class = "parental")))
})

test_that("punion", {
  a <- structure(list(integer(0), integer(0), c(1L, 2L, 4L, 5L), 2L, integer(0)), class = c("bn", "parental"))
  b <- structure(list(integer(0), integer(0), c(1L, 4L, 5L), 2L, integer(0)), class = c("bn", "parental"))
  d <- structure(list(integer(0), 3L, 1L, c(2L, 5L), integer(0)), class = c("bn", "parental"))
  x <- parental(integer(0), integer(0))
  z <- parental(integer(0), integer(0), integer(0), integer(0), integer(0))
  
  expect_that(punion(a, b), is_identical_to(structure(list(integer(0), integer(0), c(1L, 2L, 4L, 5L), 2L, integer(0)), class = "parental")))
  expect_that(punion(b, d), is_identical_to(structure(list(integer(0), 3L, c(1L, 4L, 5L), c(2L, 5L), integer(0)), class = "parental")))
  expect_that(punion(d, x), throws_error())
  expect_that(punion(d, z), is_identical_to(structure(list(integer(0), 3L, 1L, c(2L, 5L), integer(0)), class = "parental"))) # drops "bn"
})


test_that("pintersect", {
  a <- structure(list(integer(0), integer(0), c(1L, 2L, 4L, 5L), 2L, integer(0)), class = c("bn", "parental"))
  b <- structure(list(integer(0), integer(0), c(1L, 4L, 5L), 2L, integer(0)), class = c("bn", "parental"))
  d <- structure(list(integer(0), 3L, 1L, c(2L, 5L), integer(0)), class = c("bn", "parental"))
  x <- parental(integer(0), integer(0))
  z <- parental(integer(0), integer(0), integer(0), integer(0), integer(0))
  
  expect_that(pintersect(a, b), is_identical_to(structure(list(integer(0), integer(0), c(1L, 4L, 5L), 2L, integer(0)), class = "parental")))
  expect_that(pintersect(b, d), is_identical_to(structure(list(integer(0), integer(0), 1L, 2L, integer(0)), class = "parental")))
  expect_that(pintersect(d, x), throws_error())
  expect_that(pintersect(d, z), is_identical_to(structure(list(integer(0), integer(0), integer(0), integer(0), integer(0)), class = "parental"))) # drops "bn"
})

test_that("neighbourhoodSize", {
  
  expect_that(neighbourhoodSize(bn(integer(0), integer(0))),
              equals(2))
  
  expect_that(neighbourhoodSize(bn(2, integer(0))),
              equals(2))
  expect_that(neighbourhoodSize(bn(integer(0), 1)),
              equals(2))
  
  expect_that(neighbourhoodSize(bn(integer(0), 1, integer(0))),
              equals(6))

  expect_that(neighbourhoodSize(bn(integer(0), 1, 2)),
              equals(5))
              
  expect_that(neighbourhoodSize(bn(integer(0), 1, c(1, 2))),
              equals(5))

  expect_that(neighbourhoodSize(bn(integer(0), 1, 2, c(2, 3), 3)),
              equals(15))
})



test_that("routes", {
  
  expect_equal(routes(bn(integer(0))), matrix(1, nrow = 1, ncol = 1))
  
  expected <- matrix(c(1, 1,
                       0, 1), nrow = 2, ncol = 2, byrow = T)
  expect_equal(routes(bn(integer(0), 1)), expected)
  
  expected <- matrix(c(1, 0,
                       1, 1), nrow = 2, ncol = 2, byrow = T)
  expect_equal(routes(bn(2, integer(0))), expected)
  
  
  expected <- matrix(c(1, 1, 1,
                       0, 1, 1,
                       0, 0, 1), nrow = 3, ncol = 3, byrow = T)
  expect_equal(routes(bn(integer(0), 1, 2)), expected)
  
  expected <- matrix(c(1, 0, 0,
                       1, 1, 0,
                       1, 1, 1), nrow = 3, ncol = 3, byrow = T)
  expect_equal(routes(bn(2, 3, integer(0))), expected)
  
  expected <- matrix(c(1, 0, 1, 1, 2,
                       0, 1, 1, 1, 2,
                       0, 0, 1, 1, 2,
                       0, 0, 0, 1, 1,
                       0, 0, 0, 0, 1), nrow = 5, ncol = 5, byrow = T)
  expect_equal(routes(bn(integer(0), integer(0), c(1,2), 3, c(3,4))), expected)
})
