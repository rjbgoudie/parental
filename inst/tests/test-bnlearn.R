# Part of the "parental" package, http://github.com/rjbgoudie/parental
# 
# This software is distributed under the GPL-3 license.  It is free,
# open source, and has the attribution requirements (GPL Section 7) in
#   http://github.com/rjbgoudie/parental
# 
# Note that it is required that attributions are retained with each function.
#
# Copyright 2008 Robert J. B. Goudie, University of Warwick

# context("bnlearn")
# 
# test_that("bnlearn2parental - marks", {
# 
#   local({
#     if (require("bnlearn")){
#       data(marks)
#       x <- mmpc(marks)
#   
#       expect_that(
#         bnlearn2parental(x), 
#         is_identical_to(
#           structure(list(mechanics = 2:3,
#                          vectors = c(1L, 3L),
#                          algebra = c(1L, 2L, 4L, 5L),
#                          analysis = c(3L, 5L),
#                          statistics = 3:4),
#                      class = "parental",
#                      .Names = c("mechanics", "vectors", "algebra",
#                                 "analysis", "statistics"))
#           ))
#       }
#     })
# })
# 
# test_that("bnlearn2parental - learning.test gs()", {
# 
#   
#   data(learning.test)
#   bn.gs <- gs(learning.test)
#   
#   expect_that(
#     bnlearn2parental(bn.gs), 
#     is_identical_to(
#       structure(list(A = 2L,
#                      B = 1L,
#                      C = integer(0),
#                      D = c(1L, 3L),
#                      E = c(2L, 6L),
#                      F = integer(0)),
#                  class = "parental",
#                  .Names = c("A", "B", "C", "D", "E", "F"))))
# })
# 
# test_that("bnlearn2parental - learning.test hc)", {
# 
#   
#   data(learning.test)
#   bn.hc <- hc(learning.test, score = "aic")
#   
#   expect_that(
#     bnlearn2parental(bn.hc), 
#     is_identical_to(
#       structure(list(A = integer(0),
#                      B = 1L,
#                      C = integer(0),
#                      D = c(1L, 3L),
#                      E = c(2L, 6L),
#                      F = integer(0)),
#                  class = "parental",
#                  .Names = c("A", "B", "C", "D", "E", "F"))))
# })
# 
# test_that("parental2bnlearn", {
# 
#   
#   expect_that(
#     parental2bnlearn(parental(integer(0), c(1,3), integer(0))), 
#     is_identical_to(
#       structure(list(learning = structure(list(whitelist = NULL,
#                                                blacklist = NULL,
#                                                test = "none",
#                                                ntests = 0,
#                                                algo = "empty",
#                                                args = list()),
#                                            .Names = c("whitelist",
#                                                       "blacklist",
#                                                       "test",
#                                                       "ntests",
#                                                       "algo",
#                                                       "args")),
#                       nodes = structure(list(
#                                 `1` = structure(list(mb = c("2", "3"),
#                                                      nbr = "2",
#                                                      parents = character(0),
#                                                      children = "2"),
#                                                 .Names = c("mb", "nbr",
#                                                            "parents",
#                                                            "children")),
#                                 `2` = structure(list(mb = c("1", "3"), nbr = c("1", "3"
#           ), parents = c("1", "3"), children = character(0)), .Names = c("mb", 
#           "nbr", "parents", "children")), `3` = structure(list(mb = c("1", 
#           "2"), nbr = "2", parents = character(0), children = "2"), .Names = c("mb", 
#           "nbr", "parents", "children"))), .Names = c("1", "2", "3"
#       )), arcs = structure(c("1", "3", "2", "2"), .Dim = c(2L, 2L), .Dimnames = list(
#           NULL, c("from", "to")))), .Names = c("learning", "nodes", 
#       "arcs"), class = "bn")))
#   
# })
# 
# test_that("both directions - learning.test hc()", {
# 
#   
#   data(learning.test)
#   bn.hc <- hc(learning.test, score = "aic")
#   
#   # we lose the learning type data in the conversion
#   # but the other parts should be retained
#   expect_that(
#     bnlearn:::compare(bn.hc, parental2bnlearn(bnlearn2parental(bn.hc))), 
#     is_true())
# })
# 
# test_that("both directions - learning.test gs()", {
# 
#   
#   data(learning.test)
#   bn.gs <- gs(learning.test)
#   
#   # we lose the learning type data in the conversion
#   # but the other parts should be retained
#   expect_that(
#     bnlearn:::compare(bn.gs, parental2bnlearn(bnlearn2parental(bn.gs))), 
#     is_true())
# })
