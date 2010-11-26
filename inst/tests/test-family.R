context("Spaces")

test_that("enumerateBNSpace", {
  expect_that(length(enumerateBNSpace(1)), is_identical_to(1L))
  expect_that(length(enumerateBNSpace(2)), is_identical_to(3L))
  expect_that(length(enumerateBNSpace(3)), is_identical_to(25L))
})

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
  
  expect_equal(sum(sapply(enumerateParents(c(12, 4, 5)), function(x){
    identical(x, 12L)
  })), 1)
  
  expect_equal(sum(sapply(enumerateParents(c(12, 4, 5)), function(x){
    identical(x, c(5L, 12L))
  })), 1)
  
  expect_equal(sum(sapply(enumerateParents(c(12, 4, 5)), function(x){
    identical(x, c(4L, 5L, 12L))
  })), 1)
  
  # using maxNumberParents
  # this should not be in the set
  expect_equal(sum(sapply(enumerateParents(c(12, 4, 5), max = 2), function(x){
    identical(x, c(4L, 5L, 12L))
  })), 0)
  
  expect_equal(length(enumerateParents(c(12, 4, 5), max = 2)), 7)
})
