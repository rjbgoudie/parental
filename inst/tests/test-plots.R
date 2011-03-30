# Part of the "parental" package, http://github.com/rjbgoudie/parental
# 
# This software is distributed under the GPL-3 license.  It is free,
# open source, and has the attribution requirements (GPL Section 7) in
#   http://github.com/rjbgoudie/parental
# 
# Note that it is required that attributions are retained with each function.
#
# Copyright 2008 Robert J. B. Goudie, University of Warwick

context("Plots")

test_that("Basic", {
  
  x <- bn(one = integer(0), two = 1, `abc\ndef` = 2, `lllllllll` = c(1,3))
  barchart(x)
  
  
})
