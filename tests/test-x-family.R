context("Slow Spaces")

test_that("enumerateBNSpace", {
  expect_that(length(enumerateBNSpace(4)), is_identical_to(543L))
})