# Part of the "parental" package, http://github.com/rbtgde/parental
# 
# This software is distributed under the GPL-3 license.  It is free,
# open source, and has the attribution requirements (GPL Section 7) in
#   http://github.com/rbtgde/parental
# 
# Note that it is required that attributions are retained with each function.
#
# Copyright 2008 Robert J. B. Goudie, University of Warwick

context("Class parental")

test_that("Printing", {
  expect_that(
    capture.output(print(parental(2, integer(0))), file = NULL),
    equals('[1] "[2][]"'))
  expect_that(
    capture.output(print(parental(integer(0), integer(0))), file = NULL),
    equals('[1] "[][]"'))
})

test_that("Class", {
  expect_that(
    parental(integer(0), numeric(0), c(2,1)),
    is_identical_to(
      structure(list(integer(0), integer(0), 1:2), class = "parental")))
  expect_that(
    parental(integer(0), character(0), c(2,1)),
    is_identical_to(
      structure(list(integer(0), integer(0), 1:2), class = "parental")))
  expect_that(
    parental(c(), c(), c(2,1)),
    is_identical_to(
      structure(list(integer(0), integer(0), 1:2), class = "parental")))
  expect_that(
    parental(NULL, c(), c(2,1)),
    is_identical_to(
      structure(list(integer(0), integer(0), 1:2), class = "parental")))
  
  expect_that(
    parental(2, 1),
    is_identical_to(structure(list(2L, 1L), class = "parental")))
})

test_that("Validate", {
  expect_that(is.valid(parental(integer(0))), is_true())
  
  x <- list(numeric(0))
  class(x) <- "parental"
  expect_that(is.valid(x), is_false())
  
  x <- list(c(2, 1))
  class(x) <- "parental"
  expect_that(is.valid(x), is_false())
  
  x <- list(c(2L, 1L))
  class(x) <- "parental"
  expect_that(is.valid(x), is_false())
  
  x <- list(c(2L), integer(0), integer(0), integer(0), integer(0), integer(0), 
            integer(0), integer(0), integer(0), integer(0), integer(0), 
            integer(0), integer(0), integer(0), integer(0), integer(0), 
            integer(0), integer(0), integer(0), integer(0), integer(0))
  class(x) <- "parental"
  expect_that(is.valid(x), is_true())
})


test_that("reNameNodes", {
  pl <- parental.list(parental(integer(0), 1L, 2L),
                      parental(3L, 1L, integer(0)))
  
  expect_that(
    renameNodes(pl, c("one", "two", "three")),
    is_identical_to(
      structure(list(
        structure(
          list(one = integer(0), two = 1L, three = 2L), 
          class = "parental",
          .Names = c("one", "two", "three")
        ), 
        structure(
          list(one = 3L, two = 1L, three = integer(0)), 
          class = "parental", 
          .Names = c("one", "two", "three")
          )
        ), 
        class = "parental.list"
      )
    )
  )
  
  # check that bn class is retained
  pl <- parental.list(bn(integer(0), 1L, 2L), bn(3L, 1L, integer(0)))
  expect_that(
    renameNodes(pl, c("one", "two", "three")),
    is_identical_to(
      structure(list(
        structure(
          list(one = integer(0), two = 1L, three = 2L), 
          class = c("bn", "parental"),
          .Names = c("one", "two", "three")
        ), 
        structure(
          list(one = 3L, two = 1L, three = integer(0)), 
          class = c("bn", "parental"), 
          .Names = c("one", "two", "three")
          )
        ), 
        class = "parental.list"
      )
    )
  )
})

test_that("concatenation", {
  pl1 <- parental.list(parental(integer(0), 1L, 2L),
                       parental(3L, 1L, integer(0)))
  pl2 <- parental.list(parental(integer(0)), parental(integer(0), 1L))
  
  
  expect_that(
    c(pl1, pl2),
    is_identical_to(
      parental.list(parental(integer(0), 1L, 2L),
                    parental(3L, 1L, integer(0)),
                    parental(integer(0)),
                    parental(integer(0), 1L))
    )
  )
  
  pl1 <- parental.list(bn(integer(0), 1L, 2L), bn(3L, 1L, integer(0)))
  pl2 <- parental.list(bn(integer(0)), bn(integer(0), 1L))
  
  
  expect_that(
    c(pl1, pl2),
    is_identical_to(
      parental.list(bn(integer(0), 1L, 2L),
                    bn(3L, 1L, integer(0)),
                    bn(integer(0)),
                    bn(integer(0), 1L))
    )
  )
})

test_that("Empty", {
  expect_that(empty(0), throws_error())
  expect_that(empty(-1), throws_error())
  expect_that(empty(1),
              is_identical_to(parental(integer(0))))
  expect_that(empty(1L),
              is_identical_to(parental(integer(0))))
  expect_that(empty(2),
              is_identical_to(parental(integer(0), integer(0))))
  expect_that(empty(10),
              is_identical_to(parental(integer(0), integer(0), integer(0),
                                       integer(0), integer(0), integer(0),
                                       integer(0), integer(0), integer(0),
                                       integer(0))))
  expect_that(empty(2, "bvs"), is_identical_to(bvs(integer(0), integer(0))))
  expect_that(empty(2, "bn"), is_identical_to(bn(integer(0), integer(0))))
})

test_that("bvsresponse", {
  expect_that(
    bvsresponse(integer(0)), 
    throws_error())
  expect_that(
    bvsresponse(c(), response = 1, nNodes = 1), 
    throws_error())
  expect_that(
    bvsresponse(c(2L), response = 1, nNodes = 1), 
    throws_error())
  expect_that(
    bvsresponse(c(2L), response = 2, nNodes = 1), 
    throws_error())
  expect_that(
    bvsresponse(c(-1L), response = 1, nNodes = 1), 
    throws_error())
  
  expect_that(
    bvsresponse(integer(0), response = 1, nNodes = 1), 
    is_identical_to(structure(list(
      parents = integer(0), response = 1L, nNodes = 1L),
      .Names = c("parents", "response", "nNodes"), 
      class = "bvsresponse")))
  
  expect_that(
    bvsresponse(c(2L, 3L, 4L), response = 5, nNodes = 6),
    is_identical_to(
      structure(list(parents = 2:4, response = 5L, nNodes = 6L),
      .Names = c("parents", "response", "nNodes"), class = "bvsresponse")))
})

test_that("bvsresponse is.valid", {
  x <- structure(list(parents = 2:4, response = 5, nNodes = 6),
      .Names = c("parents", "response", "nNodes"), class = "bvsresponse")
  expect_that(
    is.valid(x), 
    is_true())
  
  x <- structure(list(
      parents = integer(0), response = 1, nNodes = 1),
      .Names = c("parents", "response", "nNodes"), 
      class = "bvsresponse")
  expect_that(
    is.valid(x),
    is_true())
  
  x <- structure(list(
      parents = c(1L), response = 1, nNodes = 1),
      .Names = c("parents", "response", "nNodes"), 
      class = "bvsresponse")
  expect_that(
    is.valid(x),
    is_false())

  x <- structure(list(
      parents = c(2L, 3L, 4L), response = 1L, nNodes = 3L),
      .Names = c("parents", "response", "nNodes"), 
      class = "bvsresponse")
  expect_that(
    is.valid(x),
    is_false())

  x <- structure(list(
      parents = integer(0), response = 1L),
      .Names = c("parents", "response"), 
      class = "bvsresponse")
  expect_that(
    is.valid(x),
    is_false())

  x <- structure(list(
      parents = 1.2, response = 1L, nNodes = 1L),
      .Names = c("parents", "response", "nNodes"), 
      class = "bvsresponse")
  expect_that(
    is.valid(x),
    is_false())

  
  x <- structure(list(
      parents = -1L, response = 1L, nNodes = 1L),
      .Names = c("parents", "response", "nNodes"), 
      class = "bvsresponse")
  expect_that(
    is.valid(x),
    is_false())
})