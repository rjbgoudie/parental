context("PDAG methods")

test_that("maximallyOrientEdges", {
  
  # Rule 1
  expect_that(
    maximallyOrientEdges(bn(integer(0), c(1,3), 2)), 
    is_identical_to(
      structure(list(integer(0), 1L, 2L), class = "parental")
    )
  )
  
  # Rule 2
  expect_that(
    maximallyOrientEdges(bn(2, c(1,3), 1)), 
    is_identical_to(
      structure(list(integer(0), c(1L, 3L), 1L), class = "parental")
    )
  )
  
  # Rule 3
  expect_that(
    maximallyOrientEdges(bn(c(2, 3, 4), c(1, 3, 4), 1, 1)),
    is_identical_to(
      structure(list(3:4, c(1L, 3L, 4L), 1L, 1L), class = "parental")
    )
  )
  
  # Rule 3, but with adjacent 3, 4
  expect_that(
    maximallyOrientEdges(bn(c(2, 3, 4), c(1, 3, 4), 1, c(1,3))),
    is_identical_to(
      structure(list(2:4, c(1L, 3L, 4L), 1L, c(1L, 3L)), class = "parental")
    )
  )
  
  # Rule 3, but with adjacent 3, 4
  expect_that(
    maximallyOrientEdges(bn(c(2, 3, 4), c(1, 3, 4), c(1, 4), 1)),
    is_identical_to(
      structure(list(2:4, c(1L, 3L, 4L), c(1L, 4L), 1L), class = "parental")
    )
  )
  
  # Rule 3, but with adjacent 3, 4
  expect_that(
    maximallyOrientEdges(bn(c(2, 3, 4), c(1, 3, 4), c(1, 4), c(1,3))),
    is_identical_to(
      structure(list(2:4, c(1L, 3L, 4L), c(1L, 4L), c(1L, 3L)), class = "parental")
    )
  )
  
  # Rule 4
  expect_that(
    maximallyOrientEdges(bn(c(2, 3, 4), c(1, 3, 4), 1, 1)),
    is_identical_to(
      structure(list(3:4, c(1L, 3L, 4L), 1L, 1L), class = "parental")
    )
  )
  
  # Rule 4, but with adjacenct 3, 4
  expect_that(
    maximallyOrientEdges(bn(c(2, 3, 4), c(1, 3, 4), c(1, 4), 1)),
    is_identical_to(
      structure(list(2:4, c(1L, 3L, 4L), c(1L, 4L), 1L), class = "parental")
    )
  )
  
  # Rule 4, but with adjacenct 3, 4
  expect_that(
    maximallyOrientEdges(bn(c(2, 3, 4), c(1, 3, 4), 1, c(1, 3))),
    is_identical_to(
      structure(list(2:4, c(1L, 3L, 4L), 1L, c(1L, 3L)), class = "parental")
    )
  )
  
  # Rule 4, but with adjacenct 3, 4
  expect_that(
    maximallyOrientEdges(bn(c(2, 3, 4), c(1, 3, 4), c(1, 4), c(1, 3))),
    is_identical_to(
      structure(list(2:4, c(1L, 3L, 4L), c(1L, 4L), c(1L, 3L)), class = "parental")
    )
  )
  
  # finding the dag-equivalence class from rule 4
  expect_that(
    maximallyOrientEdges(bn(c(3, 4), c(1, 3, 4), integer(0), 1)),
    is_identical_to(
      structure(list(3L, c(1L, 3L, 4L), integer(0), 1L), class = "parental")
    )
  )
  expect_that(
    maximallyOrientEdges(bn(4, c(1, 3, 4), 1, 1)),
    is_identical_to(
      structure(list(4L, c(1L, 3L, 4L), 1L, 1L), class = "parental")
    )
  )
})


test_that("pdag2alldags", {
  
  # Based on Rule 4
  expect_that(
    pdag2alldags(bn(c(2, 3, 4), c(1, 3, 4), 1, 1)),
    is_identical_to(
      structure(list(structure(list(3L, c(1L, 3L, 4L), integer(0), 1L), class = c("bn", "parental")), structure(list(4L, c(1L, 3L, 4L), 1L, integer(0)), class = c("bn", "parental")), structure(list(integer(0), c(1L, 3L, 4L), 1L, 1L), class = c("bn", "parental"))), class = c("bn.list", "parental.list"))
    )
  )
  
})
