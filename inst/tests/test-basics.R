# Part of the "parental" package, http://github.com/rjbgoudie/parental
# 
# This software is distributed under the GPL-3 license.  It is free,
# open source, and has the attribution requirements (GPL Section 7) in
#   http://github.com/rjbgoudie/parental
# 
# Note that it is required that attributions are retained with each function.
#
# Copyright 2008 Robert J. B. Goudie, University of Warwick

context("The very basics")

test_that("nNodes", {
  expect_identical(nNodes(parental()), 0L)
  expect_identical(nNodes(bn(c())), 1L)
  expect_identical(nNodes(bn(1)), 1L)
  expect_identical(nNodes(bn(2)), 1L) # nNodes doesn't check for validity
  expect_identical(nNodes(bn(2, 1)), 2L)
})
