context("BVS Basics")

test_that("Very weird input", {
  expect_that(enumerateBVSSpace(0, 1), throws_error("Number of nodes must be a positive natural number"))
  expect_that(enumerateBVSSpace(0, -1), throws_error("Number of nodes must be a positive natural number"))
  expect_that(enumerateBVSSpace(-1, -1), throws_error("Number of nodes must be a positive natural number"))
  
  expect_that(enumerateBVSSpace(1.5, 1), throws_error())
  
  expect_that(enumerateBVSSpace(list(1,2,3), 1), throws_error())
  expect_that(enumerateBVSSpace(list(1), 1), throws_error())
  
  expect_that(enumerateBVSSpace(c(1,2,3,4), 1), throws_error())
  expect_that(enumerateBVSSpace(c(10,11), 1), throws_error())
  expect_that(enumerateBVSSpace(sin(23), 1), throws_error())
  
  expect_that(enumerateBVSSpace(10, 100), throws_error())
  expect_that(enumerateBVSSpace(10, sin(23)), throws_error())
  expect_that(enumerateBVSSpace(10, 1:10), throws_error())
  expect_that(enumerateBVSSpace(10, list(1)), throws_error())
  expect_that(enumerateBVSSpace(10, list(1,2,3)), throws_error())
  
  expect_that(enumerateBVSSpace(10, "hello"), throws_error())
  
  expect_that(enumerateBVSSpace("seriously messed up", "hello"), throws_error())
  
  expect_that(enumerateBVSSpace("0", "hello"), throws_error())
  expect_that(enumerateBVSSpace(factor(c("1")), "hello"), throws_error())
  expect_that(enumerateBVSSpace(factor(c("2")), 1), throws_error())
  
  expect_that(enumerateBVSSpace("1", 1), throws_error())
})

test_that("1 Node input", {
  expect_that(enumerateBVSSpace(1, -1), throws_error("Response must be a positive natural number"))
  
  expect_that(enumerateBVSSpace(1, 0), throws_error("Response must be a positive natural number"))
  
  expect_that(enumerateBVSSpace(1, 1), is_identical_to(structure(list(structure(list(integer(0)), class = c("bvs", "bn", "parental"))), class = c("bvs.list", "bn.list", "parental.list"))))
  expect_that(enumerateBVSSpace(1, 2), throws_error("response must be a node"))
})

test_that("2 Node input", {
  expect_that(enumerateBVSSpace(2, 1), is_identical_to(structure(list(structure(list(2L, integer(0)), class = c("bvs", "bn", "parental")), structure(list(integer(0), integer(0)), class = c("bvs", "bn", "parental"))), class = c("bvs.list", "bn.list", "parental.list"))))
  
  expect_that(enumerateBVSSpace(2, 2), is_identical_to(structure(list(structure(list(integer(0), 1L), class = c("bvs", "bn", "parental")), structure(list(integer(0), integer(0)), class = c("bvs", "bn", "parental"))), class = c("bvs.list", "bn.list", "parental.list"))))
})

test_that("3 Node input", {
  expect_that(enumerateBVSSpace(3, 1), is_identical_to(
    structure(list(structure(list(2L, integer(0), integer(0)), class = c("bvs", 
"bn", "parental")), structure(list(3L, integer(0), integer(0)), class = c("bvs", 
"bn", "parental")), structure(list(2:3, integer(0), integer(0)), class = c("bvs", 
"bn", "parental")), structure(list(integer(0), integer(0), integer(0)), class = c("bvs", 
"bn", "parental"))), class = c("bvs.list", "bn.list", "parental.list"
))
  ))
  
  expect_that(enumerateBVSSpace(3, 2), is_identical_to(structure(list(
    structure(list(integer(0), 1L, integer(0)), class = c("bvs", 
"bn", "parental")),
    structure(list(integer(0), 3L, integer(0)), class = c("bvs", 
"bn", "parental")),
    structure(list(integer(0), c(1L,3L), integer(0)), class = c("bvs", 
"bn", "parental")), 
    structure(list(integer(0), integer(0), integer(0)), class = c("bvs", 
"bn", "parental"))
  ), class = c("bvs.list", "bn.list", "parental.list"))))
  
    expect_that(enumerateBVSSpace(3, 3), is_identical_to(structure(list(
      structure(list(integer(0), integer(0), 1L), class = c("bvs", 
"bn", "parental")),
      structure(list(integer(0), integer(0), 2L), class = c("bvs", 
"bn", "parental")),
      structure(list(integer(0), integer(0), c(1L,2L)), class = c("bvs", 
"bn", "parental")), 
      structure(list(integer(0), integer(0), integer(0)), class = c("bvs", 
"bn", "parental"))
    ), class = c("bvs.list", "bn.list", "parental.list"))))
    
    expect_that(enumerateBVSSpace(3, 1, maxNumberParents = 1), is_identical_to(structure(list(
      structure(list(c(2L), integer(0), integer(0)), class = c("bvs", 
"bn", "parental")),
      structure(list(c(3L), integer(0), integer(0)), class = c("bvs", 
"bn", "parental")),
      structure(list(integer(0), integer(0), integer(0)), class = c("bvs", 
"bn", "parental"))
    ), class = c("bvs.list", "bn.list", "parental.list"))))
    
    expect_that(enumerateBVSSpace(3, 1, maxNumberParents = 2), is_identical_to(structure(list(
      structure(list(c(2L), integer(0), integer(0)), class = c("bvs", 
"bn", "parental")),
      structure(list(c(3L), integer(0), integer(0)), class = c("bvs", 
"bn", "parental")),
      structure(list(c(2L,3L), integer(0), integer(0)), class = c("bvs", 
"bn", "parental")), 
      structure(list(integer(0), integer(0), integer(0)), class = c("bvs", 
"bn", "parental"))
    ), class = c("bvs.list", "bn.list", "parental.list"))))
    
    expect_that(enumerateBVSSpace(3, 3, maxNumberParents = 3), gives_warning("maxNumberParents is too high"))
    expect_that(enumerateBVSSpace(3, 10), throws_error())
    expect_that(enumerateBVSSpace(3, 4), throws_error())
})
  
test_that("4 Nod          e input", {
    expect_that(          enumerateBVSSpace(4, 1), is_identical_to(structure(list(
      structure(list(c(2L), integer(0), integer(0), integer(0)), class = c("bvs", 
"bn", "parental")),
      structure(list(c(3L), integer(0), integer(0), integer(0)), class = c("bvs", 
"bn", "parental")),
      structure(list(c(4L), integer(0), integer(0), integer(0)), class = c("bvs", 
"bn", "parental")),
      structure(list(c(2L, 3L), integer(0), integer(0), integer(0)), class = c("bvs", 
"bn", "parental")),
      structure(list(c(2L, 4L), integer(0), integer(0), integer(0)), class = c("bvs", 
"bn", "parental")),
      structure(list(c(3L, 4L), integer(0), integer(0), integer(0)), class = c("bvs", 
"bn", "parental")),
      structure(list(c(2L, 3L, 4L), integer(0), integer(0), integer(0)), class = c("bvs", 
"bn", "parental")),
      structure(list(integer(0), integer(0), integer(0), integer(0)), class = c("bvs", 
"bn", "parental"))
    ), class = c("bvs.list", "bn.list", "parental.list"))))
    
    expect_that(          enumerateBVSSpace(4, 1, maxNumberParents = 3), is_identical_to(structure(list(
      structure(list(c(2L), integer(0), integer(0), integer(0)), class = c("bvs", 
"bn", "parental")),
      structure(list(c(3L), integer(0), integer(0), integer(0)), class = c("bvs", 
"bn", "parental")),
      structure(list(c(4L), integer(0), integer(0), integer(0)), class = c("bvs", 
"bn", "parental")),
      structure(list(c(2L, 3L), integer(0), integer(0), integer(0)), class = c("bvs", 
"bn", "parental")),
      structure(list(c(2L, 4L), integer(0), integer(0), integer(0)), class = c("bvs", 
"bn", "parental")),
      structure(list(c(3L, 4L), integer(0), integer(0), integer(0)), class = c("bvs", 
"bn", "parental")),
      structure(list(c(2L, 3L, 4L), integer(0), integer(0), integer(0)), class = c("bvs", 
"bn", "parental")),
      structure(list(integer(0), integer(0), integer(0), integer(0)), class = c("bvs", 
"bn", "parental"))
    ), class = c("bvs.list", "bn.list", "parental.list"))))
    
    expect_that(          enumerateBVSSpace(4, 1, maxNumberParents = 2), is_identical_to(structure(list(
      structure(list(c(2L), integer(0), integer(0), integer(0)), class = c("bvs", 
"bn", "parental")),
      structure(list(c(3L), integer(0), integer(0), integer(0)), class = c("bvs", 
"bn", "parental")),
      structure(list(c(4L), integer(0), integer(0), integer(0)), class = c("bvs", 
"bn", "parental")),
      structure(list(c(2L, 3L), integer(0), integer(0), integer(0)), class = c("bvs", 
"bn", "parental")),
      structure(list(c(2L, 4L), integer(0), integer(0), integer(0)), class = c("bvs", 
"bn", "parental")),
      structure(list(c(3L, 4L), integer(0), integer(0), integer(0)), class = c("bvs", 
"bn", "parental")),
      structure(list(integer(0), integer(0), integer(0), integer(0)), class = c("bvs", 
"bn", "parental"))
    ), class = c("bvs.list", "bn.list", "parental.list"))))
    
    expect_that(          enumerateBVSSpace(4, 1, maxNumberParents = 1), is_identical_to(structure(list(
      structure(list(c(2L), integer(0), integer(0), integer(0)), class = c("bvs", 
"bn", "parental")),
      structure(list(c(3L), integer(0), integer(0), integer(0)), class = c("bvs", 
"bn", "parental")),
      structure(list(c(4L), integer(0), integer(0), integer(0)), class = c("bvs", 
"bn", "parental")),
      structure(list(integer(0), integer(0), integer(0), integer(0)), class = c("bvs", 
"bn", "parental"))
    ), class = c("bvs.list", "bn.list", "parental.list"))))
    
    expect_that(          enumerateBVSSpace(4, 1, maxNumberParents = 0), is_identical_to(structure(list(
      structure(list(integer(0), integer(0), integer(0), integer(0)), class = c("bvs", 
"bn", "parental"))
    ), class = c("bvs.list", "bn.list", "parental.list"))))
    
    expect_that(          enumerateBVSSpace(4, 2), is_identical_to(structure(list(
      structure(list(integer(0),  c(1L),    integer(0),  integer(0)), class = c("bvs", "bn", "parental")),
      structure(list(integer(0),  c(3L),    integer(0),  integer(0)), class = c("bvs", "bn", "parental")),
      structure(list(integer(0),  c(4L),    integer(0),  integer(0)), class = c("bvs", "bn", "parental")),
      structure(list(integer(0),  c(1L,3L),      integer(0),  integer(0)), class = c("bvs", "bn", "parental")),
      structure(list(integer(0),  c(1L,4L),      integer(0),  integer(0)), class = c("bvs", "bn", "parental")),
      structure(list(integer(0),  c(3L,4L),      integer(0),  integer(0)), class = c("bvs", "bn", "parental")),
      structure(list(integer(0),  c(1L,3L,4L),  integer(0),  integer(0)), class = c("bvs", "bn", "parental")),
      structure(list(integer(0),  integer(0),  integer(0),  integer(0)), class = c("bvs", 
"bn", "parental"))
    ), class = c("bvs.list", "bn.list", "parental.list"))))
    
    expect_that(          enumerateBVSSpace(4, 3), is_identical_to(structure(list(
      structure(list(integer(0),  integer(0),  c(1L),    integer(0)), class = c("bvs", "bn", "parental")),
      structure(list(integer(0),  integer(0),  c(2L),    integer(0)), class = c("bvs", "bn", "parental")),
      structure(list(integer(0),  integer(0),  c(4L),    integer(0)), class = c("bvs", "bn", "parental")),
      structure(list(integer(0),  integer(0),  c(1L,2L),      integer(0)), class = c("bvs", "bn", "parental")),
      structure(list(integer(0),  integer(0),  c(1L,4L),      integer(0)), class = c("bvs", "bn", "parental")),
      structure(list(integer(0),  integer(0),  c(2L,4L),      integer(0)), class = c("bvs", "bn", "parental")),
      structure(list(integer(0),  integer(0),  c(1L,2L,4L),  integer(0)), class = c("bvs", "bn", "parental")),
      structure(list(integer(0),  integer(0),  integer(0),  integer(0)), class = c("bvs", 
"bn", "parental"))
    ), class = c("bvs.list", "bn.list", "parental.list"))))
    
    expect_that(          enumerateBVSSpace(4, 4), is_identical_to(structure(list(
      structure(list(integer(0),  integer(0), integer(0),  c(1L)    ), class = c("bvs", "bn", "parental")),
      structure(list(integer(0),  integer(0), integer(0),  c(2L)    ), class = c("bvs", "bn", "parental")),
      structure(list(integer(0),  integer(0), integer(0),  c(3L)    ), class = c("bvs", "bn", "parental")),
      structure(list(integer(0),  integer(0), integer(0),  c(1L,2L)      ), class = c("bvs", "bn", "parental")),
      structure(list(integer(0),  integer(0), integer(0),  c(1L,3L)      ), class = c("bvs", "bn", "parental")),
      structure(list(integer(0),  integer(0), integer(0),  c(2L,3L)      ), class = c("bvs", "bn", "parental")),
      structure(list(integer(0),  integer(0), integer(0),  c(1L,2L,3L)  ), class = c("bvs", 
"bn", "parental")),
      structure(list(integer(0),  integer(0), integer(0),  integer(0)  ), class = c("bvs", 
"bn", "parental"))
    ), class = c("bvs.list", "bn.list", "parental.list"))))
})
