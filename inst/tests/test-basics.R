context("The very basics")

test_that("nNodes", {
  expect_identical(nNodes(parental()), 0L)
  expect_identical(nNodes(bn(c())), 1L)
  expect_identical(nNodes(bn(1)), 1L)
  expect_identical(nNodes(bn(2)), 1L) # nNodes doesn't check for validity
  expect_identical(nNodes(bn(2, 1)), 2L)
})
