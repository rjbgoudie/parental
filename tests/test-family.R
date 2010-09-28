context("Spaces")

test_that("enumerateBNSpace", {
  expect_that(length(enumerateBNSpace(1)), is_identical_to(1L))
  expect_that(length(enumerateBNSpace(2)), is_identical_to(3L))
  expect_that(length(enumerateBNSpace(3)), is_identical_to(25L))
})
