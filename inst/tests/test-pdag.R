# Part of the "parental" package, http://github.com/rbtgde/parental
# 
# This software is distributed under the GPL-3 license.  It is free,
# open source, and has the attribution requirements (GPL Section 7) in
#   http://github.com/rbtgde/parental
# 
# Note that it is required that attributions are retained with each function.
#
# Copyright 2008 Robert J. B. Goudie, University of Warwick

context("PDAG methods")

test_that("maximallyOrientEdges", {
  
  # Rule 1
  expect_that(
    maximallyOrientEdges(bn(integer(0), c(1,3), 2)), 
    is_identical_to(
      structure(list(integer(0), 1L, 2L), class = "parental")
    )
  )
  
  # Rule 2
  expect_that(
    maximallyOrientEdges(bn(2, c(1,3), 1)), 
    is_identical_to(
      structure(list(integer(0), c(1L, 3L), 1L), class = "parental")
    )
  )
  
  # Rule 3
  expect_that(
    maximallyOrientEdges(bn(c(2, 3, 4), c(1, 3, 4), 1, 1)),
    is_identical_to(
      structure(list(3:4, c(1L, 3L, 4L), 1L, 1L), class = "parental")
    )
  )
  
  # Rule 3, but with adjacent 3, 4
  expect_that(
    maximallyOrientEdges(bn(c(2, 3, 4), c(1, 3, 4), 1, c(1,3))),
    is_identical_to(
      structure(list(2:4, c(1L, 3L, 4L), 1L, c(1L, 3L)), class = "parental")
    )
  )
  
  # Rule 3, but with adjacent 3, 4
  expect_that(
    maximallyOrientEdges(bn(c(2, 3, 4), c(1, 3, 4), c(1, 4), 1)),
    is_identical_to(
      structure(list(2:4, c(1L, 3L, 4L), c(1L, 4L), 1L), class = "parental")
    )
  )
  
  # Rule 3, but with adjacent 3, 4
  expect_that(
    maximallyOrientEdges(bn(c(2, 3, 4), c(1, 3, 4), c(1, 4), c(1,3))),
    is_identical_to(
      structure(list(2:4, c(1L, 3L, 4L), c(1L, 4L), c(1L, 3L)), class = "parental")
    )
  )
  
  # Rule 4
  expect_that(
    maximallyOrientEdges(bn(c(2, 3, 4), c(1, 3, 4), 1, 1)),
    is_identical_to(
      structure(list(3:4, c(1L, 3L, 4L), 1L, 1L), class = "parental")
    )
  )
  
  # Rule 4, but with adjacenct 3, 4
  expect_that(
    maximallyOrientEdges(bn(c(2, 3, 4), c(1, 3, 4), c(1, 4), 1)),
    is_identical_to(
      structure(list(2:4, c(1L, 3L, 4L), c(1L, 4L), 1L), class = "parental")
    )
  )
  
  # Rule 4, but with adjacenct 3, 4
  expect_that(
    maximallyOrientEdges(bn(c(2, 3, 4), c(1, 3, 4), 1, c(1, 3))),
    is_identical_to(
      structure(list(2:4, c(1L, 3L, 4L), 1L, c(1L, 3L)), class = "parental")
    )
  )
  
  # Rule 4, but with adjacenct 3, 4
  expect_that(
    maximallyOrientEdges(bn(c(2, 3, 4), c(1, 3, 4), c(1, 4), c(1, 3))),
    is_identical_to(
      structure(list(2:4, c(1L, 3L, 4L), c(1L, 4L), c(1L, 3L)), class = "parental")
    )
  )
  
  # finding the dag-equivalence class from rule 4
  expect_that(
    maximallyOrientEdges(bn(c(3, 4), c(1, 3, 4), integer(0), 1)),
    is_identical_to(
      structure(list(3L, c(1L, 3L, 4L), integer(0), 1L), class = "parental")
    )
  )
  expect_that(
    maximallyOrientEdges(bn(4, c(1, 3, 4), 1, 1)),
    is_identical_to(
      structure(list(4L, c(1L, 3L, 4L), 1L, 1L), class = "parental")
    )
  )
})


test_that("pdag2alldags", {
  
  # Based on Rule 4
  expect_that(
    pdag2alldags(bn(c(2, 3, 4), c(1, 3, 4), 1, 1)),
    is_identical_to(
      structure(list(structure(list(3L, c(1L, 3L, 4L), integer(0), 1L), class = c("bn", "parental")), structure(list(4L, c(1L, 3L, 4L), 1L, integer(0)), class = c("bn", "parental")), structure(list(integer(0), c(1L, 3L, 4L), 1L, 1L), class = c("bn", "parental"))), class = c("bn.list", "parental.list"))
    )
  )
  
})


test_that("pdag2alldags - C and R code consistent", {
  
  # Based on Rule 4
  expect_that(
    pdag2alldags(bn(c(2, 3, 4), c(1, 3, 4), 1, 1)),
    is_identical_to(
      pdag2alldagsSlow(bn(c(2, 3, 4), c(1, 3, 4), 1, 1))
    )
  )
  
})

test_that("maximallyOrientEdges", {
  
  # Rule 1
  expect_that(
    maximallyOrientEdges(bn(integer(0), c(1,3), 2)), 
    is_identical_to(
      maximallyOrientEdgesSlow(bn(integer(0), c(1,3), 2))
    )
  )
  
  # Rule 2
  expect_that(
    maximallyOrientEdges(bn(2, c(1,3), 1)), 
    is_identical_to(
      maximallyOrientEdgesSlow(bn(2, c(1,3), 1))
    )
  )
  
  # Rule 3
  expect_that(
    maximallyOrientEdges(bn(c(2, 3, 4), c(1, 3, 4), 1, 1)),
    is_identical_to(
      maximallyOrientEdgesSlow(bn(c(2, 3, 4), c(1, 3, 4), 1, 1))
    )
  )
  
  # Rule 3, but with adjacent 3, 4
  expect_that(
    maximallyOrientEdges(bn(c(2, 3, 4), c(1, 3, 4), 1, c(1,3))),
    is_identical_to(
      maximallyOrientEdgesSlow(bn(c(2, 3, 4), c(1, 3, 4), 1, c(1,3)))
    )
  )
  
  # Rule 3, but with adjacent 3, 4
  expect_that(
    maximallyOrientEdges(bn(c(2, 3, 4), c(1, 3, 4), c(1, 4), 1)),
    is_identical_to(
      maximallyOrientEdgesSlow(bn(c(2, 3, 4), c(1, 3, 4), c(1, 4), 1))
    )
  )
  
  # Rule 3, but with adjacent 3, 4
  expect_that(
    maximallyOrientEdges(bn(c(2, 3, 4), c(1, 3, 4), c(1, 4), c(1,3))),
    is_identical_to(
      maximallyOrientEdgesSlow(bn(c(2, 3, 4), c(1, 3, 4), c(1, 4), c(1,3)))
    )
  )
  
  # Rule 4
  expect_that(
    maximallyOrientEdges(bn(c(2, 3, 4), c(1, 3, 4), 1, 1)),
    is_identical_to(
      maximallyOrientEdgesSlow(bn(c(2, 3, 4), c(1, 3, 4), 1, 1))
    )
  )
  
  # Rule 4, but with adjacenct 3, 4
  expect_that(
    maximallyOrientEdges(bn(c(2, 3, 4), c(1, 3, 4), c(1, 4), 1)),
    is_identical_to(
      maximallyOrientEdgesSlow(bn(c(2, 3, 4), c(1, 3, 4), c(1, 4), 1))
    )
  )
  
  # Rule 4, but with adjacenct 3, 4
  expect_that(
    maximallyOrientEdges(bn(c(2, 3, 4), c(1, 3, 4), 1, c(1, 3))),
    is_identical_to(
      maximallyOrientEdgesSlow(bn(c(2, 3, 4), c(1, 3, 4), 1, c(1, 3)))
    )
  )
  
  # Rule 4, but with adjacenct 3, 4
  expect_that(
    maximallyOrientEdges(bn(c(2, 3, 4), c(1, 3, 4), c(1, 4), c(1, 3))),
    is_identical_to(
      maximallyOrientEdgesSlow(bn(c(2, 3, 4), c(1, 3, 4), c(1, 4), c(1, 3)))
    )
  )
  
  # finding the dag-equivalence class from rule 4
  expect_that(
    maximallyOrientEdges(bn(c(3, 4), c(1, 3, 4), integer(0), 1)),
    is_identical_to(
      maximallyOrientEdgesSlow(bn(c(3, 4), c(1, 3, 4), integer(0), 1))
    )
  )
  expect_that(
    maximallyOrientEdges(bn(4, c(1, 3, 4), 1, 1)),
    is_identical_to(
      maximallyOrientEdgesSlow(bn(4, c(1, 3, 4), 1, 1))
    )
  )
})

test_that("Supposed problem?", {
  net <- structure(list(4L, integer(0), 9L, integer(0), integer(0), c(11L,
  26L), integer(0), integer(0), 5L, 11L, c(22L, 28L), 9L, 5L, integer(0),
  integer(0), 24L, integer(0), c(26L, 29L), c(11L, 28L), integer(0),
  11L, 25L, integer(0), 30L, integer(0), c(22L, 28L), 21L,
  integer(0), c(22L, 28L), 6L), class = c("bn", "parental"))
  
  expect_that(
    maximallyOrientEdges(net),
    is_identical_to(maximallyOrientEdgesSlow(net)))
  
  expect_that(
    pdag2alldags(net),
    is_identical_to(pdag2alldagsSlow(net)))
})