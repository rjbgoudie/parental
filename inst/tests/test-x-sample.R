# Part of the "parental" package, http://github.com/rjbgoudie/parental
# 
# This software is distributed under the GPL-3 license.  It is free,
# open source, and has the attribution requirements (GPL Section 7) in
#   http://github.com/rjbgoudie/parental
# 
# Note that it is required that attributions are retained with each function.
#
# Copyright 2008 Robert J. B. Goudie, University of Warwick

context("Slow Sample")

test_that("simulate2", {
  net <- bn(c(3, 4), NULL, 4, 2)
  
  # 2 --> 4 --> 1
  #       |     ^
  #       3 ----|
  
  
  cpt <- list(
    as.table(array(c(
            # prob of 1 then 2 given
      0.7, 0.3, # 3 = 1, 4 = 1
      0.6, 0.4, # 3 = 2, 4 = 1
      0.3, 0.6, # 3 = 1, 4 = 2
      0.3, 0.7  # 3 = 2, 4 = 2
    ), c(2, 2, 2))),
    
    as.table(array(c(
      0.6, # prob of 1
      0.4  # prob of 2
    ), 2)), 
    
    as.table(array(c(
                # prob of 1 then 2 given
      0.6, 0.4, # 4 = 1
      0.4, 0.6  # 4 = 2
    ), c(2, 2))),
    
    as.table(array(c(
                # prob of 1 then 2 given
      0.6, 0.4, # 2 = 1
      0.4, 0.6  # 2 = 2
    ), c(2, 2)))
  )
  sim <- simulate(object = net, nsim = 1000, ptables = cpt)
  
  tcol <- function(i) as.vector(table(sim[, i]))
  
  expect_that(tcol(2)[1], is_within(600, 50))
  expect_that(tcol(4)[1], is_within(520, 50))
})
