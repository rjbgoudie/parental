# Part of the "parental" package, http://github.com/rjbgoudie/parental
# 
# This software is distributed under the GPL-3 license.  It is free,
# open source, and has the attribution requirements (GPL Section 7) in
#   http://github.com/rjbgoudie/parental
# 
# Note that it is required that attributions are retained with each function.
#
# Copyright 2008 Robert J. B. Goudie, University of Warwick

context("Convert")

test_that("toAdjacencyMatrix", {

  expect_that(as.adjacency(bn(NULL)),
              is_identical_to(matrix(0, nrow = 1, ncol = 1)))
  
  expect_that(as.adjacency(bn(numeric(0), 1)),
              is_identical_to(matrix(c(0,0,1,0), nrow = 2, ncol = 2)))
})


test_that("fromEdgeList", {
  
  edgelist <- matrix(c(1,2), nrow = 1, ncol = 2)
  colnames(edgelist) <- c("row", "col")
  expect_that(as.parental(edgelist, type = "edgelist", n = 2),
              is_identical_to(structure(list(integer(0), 1L),
                                        class = "parental")))
  
  edgelist <- matrix(c(2,1), nrow = 1, ncol = 2)
  colnames(edgelist) <- c("row", "col")
  expect_that(as.parental(edgelist, type = "edgelist", n = 2),
              is_identical_to(structure(list(2L, integer(0)),
                                        class = "parental")))
  
  edgelist <- matrix(nrow = 0, ncol = 2)
  colnames(edgelist) <- c("row", "col")
  expect_that(as.parental(edgelist, type = "edgelist", n = 2),
              is_identical_to(structure(list(integer(0), integer(0)),
                                        class = "parental")))
})


test_that("as.parental.character pretty", {
  expect_that(as.parental("[]", pretty = T),
              is_identical_to(parental(integer(0))))
  expect_that(as.parental("[][]", pretty = T),
              is_identical_to(parental(integer(0), integer(0))))
  
  expect_that(as.parental("[2][]", pretty = T),
              is_identical_to(parental(2, integer(0))))
  
  expect_that(as.parental("[][][1,2]", pretty = T),
              is_identical_to(parental(integer(0), integer(0), c(1,2))))
  
  expect_that(as.parental("[2][3][1]", pretty = T),
              is_identical_to(parental(2, 3, c(1))))
})

test_that("as.parental.character", {
  expect_that(as.parental("integer(0)"),
              is_identical_to(parental(integer(0))))
  expect_that(as.parental("integer(0),integer(0)"),
              is_identical_to(parental(integer(0), integer(0))))
  
  expect_that(as.parental("2,integer(0)"),
              is_identical_to(parental(2L, integer(0))))
  
  expect_that(as.parental("integer(0),integer(0),c(1,2)"),
              is_identical_to(parental(integer(0), integer(0), c(1L,2L))))
  
  expect_that(as.parental("2,3,1"),
              is_identical_to(parental(2L, 3L, c(1L))))
})

test_that("as.character.parental", {
  
  net <- bn(c(7L, 9L), c(22L, 7L), c(22L, 7L), 9L, 24L, 3:4, integer(0), 
  c(19L, 3L), 5L, c(18L, 15L, 19L), 5L, c(7L, 3L), c(7L, 3L
  ), 2L, c(2L, 16L, 24L), c(3L, 9L, 4L), c(19L, 22L), c(17L, 
  22L), c(11L, 1L, 9L), c(19L, 11L, 18L), c(1L, 7L), 7L, c(15L, 
  17L, 9L), 3L, 5L)
  
  expect_that(as.character(net), 
    equals("c(7,9),c(7,22),c(7,22),9,24,3:4,integer(0),c(3,19),5,c(15,18,19),5,c(3,7),c(3,7),2,c(2,16,24),c(3,4,9),c(19,22),c(17,22),c(1,9,11),c(11,18,19),c(1,7),7,c(9,15,17),3,5")
  )
  
  expect_that(as.character(net, pretty = T), 
    is_identical_to(
    "[7,9][7,22][7,22][9][24][3,4][][3,19][5][15,18,19][5][3,7][3,7][2][2,16,24][3,4,9][19,22][17,22][1,9,11][11,18,19][1,7][7][9,15,17][3][5]"
    )
  )
  
  expect_that(as.character(parental(integer(0)), pretty = T), 
    is_identical_to(
    "[]"
    )
  )
  
  expect_that(as.character(bn(integer(0)), pretty = T), 
    is_identical_to(
    "[]"
    )
  )
  
  expect_that(as.character(bn(integer(0), 1), pretty = T), 
    is_identical_to(
    "[][1]"
    )
  )
})

test_that("as.character.parental2", {
  net <- bn(c(7L, 9L), c(22L, 7L), c(22L, 7L), 9L, 24L, 3:4, integer(0), 
  c(19L, 3L), 5L, c(18L, 15L, 19L), 5L, c(7L, 3L), c(7L, 3L
  ), 2L, c(2L, 16L, 24L), c(3L, 9L, 4L), c(19L, 22L), c(17L, 
  22L), c(11L, 1L, 9L), c(19L, 11L, 18L), c(1L, 7L), 7L, c(15L, 
  17L, 9L), 3L, 5L)
  
  expect_that(as.character(bn(integer(0), 1)), 
    is_identical_to(
    "integer(0),1"
    )
  )
  
  # names are not dealt with for now
  expect_that(as.character(bn(one = integer(0), two = 1), pretty = T), 
    is_identical_to(
    "[][1]"
    )
  )
  
  expect_that(as.character(
    bn(integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0)), pretty = T), 
    is_identical_to(
    "[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]"
    )
  )
  
    expect_that(as.character(
    bn(integer(0), integer(0), integer(0), integer(0), integer(0), 
       integer(0), integer(0), integer(0), integer(0), integer(0), 
       integer(0), integer(0), integer(0), integer(0), integer(0), 
       integer(0), integer(0), integer(0), integer(0), integer(0), 
       integer(0), integer(0), integer(0), integer(0), integer(0), 
       integer(0), integer(0), integer(0), integer(0), integer(0), 
       integer(0), integer(0), integer(0), integer(0), integer(0), 
       integer(0), integer(0), integer(0), integer(0), integer(0), 
       integer(0), integer(0), integer(0), integer(0), integer(0), 
       integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0))), 
    is_identical_to(
    "integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0),integer(0)"
    )
  )
  
    expect_that(as.character(
    bn(integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0),integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0),integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0),integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0),integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0)), pretty = T), 
    is_identical_to(
    "[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]"
    )
  )
  
  
  expect_that(as.character(parental.list(bn(integer(0), 1), bn(integer(0), 1))), 
    is_identical_to(
    c("integer(0),1", "integer(0),1")
    )
  )
  
  expect_that(as.character(
    parental.list(
      bn(integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0)),
      bn(integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0), integer(0)))
      , pretty = T), 
    is_identical_to(
    c("[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]", "[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]")
    )
  )
  
  
  expect_that(
    as.character(parental.list(parental(2L, 3L, integer(0)), parental(integer(0), 1L, 1L))),
    is_identical_to(c("2,3,integer(0)", "integer(0),1,1"))
  )
  
  expect_that(
    as.character(parental.list(parental(2L, 3L, integer(0)), parental(integer(0), 1L, 1L)), pretty = T),
    is_identical_to(c("[2][3][]", "[][1][1]"))
  )
})


test_that("as.parental/graph", {
  require(graph)
  
  # note this includes self-loops
  set.seed(123)
  c1 <- c(1, 1, 2, 4)
  names(c1) <- as.character(1:4)
  g1 <- randomNodeGraph(c1)
  
  expect_that(as.parental(g1), is_identical_to(parental(4L, 4L, 3L, 4L)))
  expect_that(edges(as.graph(parental(4L, 4L, 3L, 4L))), is_identical_to(edges(g1)))
  
  
  p0 <- parental(integer(0))
  
  V <- as.character(1)
  edL2 <- vector("list", length = 1)
  edL2 <- list(
    list(edges = c())
  )
  names(edL2) <- V
  g0 <- new("graphNEL", nodes = V, edgeL = edL2, edgemode = "directed")
  
  expect_that(as.parental(g0), is_identical_to(p0))
  expect_that(edges(as.graph(p0)), is_identical_to(edges(g0)))
  
  V <- as.character(1:3)
  edL2 <- vector("list", length = 3)
  edL2 <- list(
    list(edges = c(2)),
    list(edges = c(3)),
    list(edges = c())
  )
  names(edL2) <- V
  g1 <- new("graphNEL", nodes = V, edgeL = edL2, edgemode = "directed")
  
  expect_that(edges(as.graph(bn(integer(0), 1, 2))), is_identical_to(edges(g1)))
})

test_that("bvsresponse", {
  # standard
  expect_that(
    as.bvs(bvsresponse(c(1L, 2L), response = 3L, nNodes = 3L)),
    is_identical_to(bvs(integer(0), integer(0), c(1L, 2L))))
  
  expect_that(
    as.bvsresponse(bvs(integer(0), c(1L, 3L), integer(0))),
    is_identical_to(bvsresponse(c(1L, 3L), response = 2, nNodes = 3)))
  
  # empty graph
  expect_that(
    as.bvsresponse(bvs(integer(0), integer(0), integer(0))),
    throws_error())
  
  expect_that(
    as.bvsresponse(bvs(integer(0), integer(0), integer(0)), response = 2),
    is_identical_to(bvsresponse(c(integer(0)), response = 2, nNodes = 3)))
  
  # response in parents
  expect_that(
    as.bvsresponse(bvs(3L, c(1L, 3L), integer(0))),
    throws_error())
  
  # largish graph
  expect_that(
    as.bvsresponse(bvs(integer(0), integer(0), integer(0),
                       integer(0), integer(0), integer(0),
                       integer(0), integer(0), integer(0),
                       integer(0), integer(0), integer(0),
                       integer(0), integer(0), integer(0),
                       integer(0), integer(0), integer(0),
                       integer(0), integer(0), integer(0), # 3 x 7 
                       1:21)),
    is_identical_to(bvsresponse(c(1:21), 
                                response = 22,
                                nNodes   = 22)))
                                

  expect_that(
    as.bvsresponse(bvs(integer(0), integer(0), integer(0),
                       integer(0), integer(0), integer(0),
                       integer(0), integer(0), integer(0),
                       integer(0), integer(0), integer(0),
                       integer(0), integer(0), integer(0),
                       integer(0), integer(0), integer(0),
                       integer(0), integer(0), integer(0),
                       integer(0), integer(0), integer(0),
                       integer(0), integer(0), integer(0),
                       integer(0), integer(0), integer(0),
                       integer(0), integer(0), integer(0),
                       integer(0), integer(0), integer(0),
                       integer(0), integer(0), integer(0),
                       integer(0), integer(0), integer(0),
                       integer(0), integer(0), integer(0),
                       integer(0), integer(0), integer(0),
                       integer(0), integer(0), integer(0),
                       integer(0), integer(0), integer(0),
                       integer(0), integer(0), integer(0),
                       integer(0), integer(0), integer(0),
                       integer(0), integer(0), integer(0), # 63
                       1:62)),
    is_identical_to(bvsresponse(c(1:62), 
                                response = 64,
                                nNodes   = 64)))
})

test_that("bvsresponse as.character", {
  expect_that(
    as.character(bvsresponse(c(1L, 2L), response = 3L, nNodes =3L)),
    is_identical_to("1,2"))
    
  expect_that(
    as.character(bvsresponse(integer(0), response = 3L, nNodes = 3L)),
    is_identical_to(""))
  
  expect_that(
    as.character(bvsresponse.list(
      bvsresponse(c(1L, 2L), response = 3L, nNodes =3L), 
      bvsresponse(c(1L, 2L), response = 3L, nNodes =3L))),
    is_identical_to(c("1,2", "1,2")))

  expect <- paste("1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,",
                  "23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,",
                  "42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,",
                  "61,62", sep = "")
  expect_that(
    as.character(bvsresponse(c(1:62), 
                             response = 64,
                             nNodes   = 64)),
    is_identical_to(expect))
})
