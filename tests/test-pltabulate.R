context("pltabulate")

test_that("pltabulate", {
  
  twoempty <- parental.list(parental(integer(0)), parental(integer(0)))
  expect_that(pltabulate(twoempty), is_identical_to(structure(2L, .Dim = 1L, .Dimnames = list("integer(0)"), class = "table")))
  expect_that(pltabulate(twoempty, pretty = T), is_identical_to(structure(2L, .Dim = 1L, .Dimnames = list("[]"), class = "table")))
  
  # using levels
  three <- parental.list(parental(integer(0), 1), parental(integer(0), 1), parental(integer(0), integer(0)))
  expect_that(pltabulate(three, levels = c("integer(0),1", "2,integer(0)", "integer(0),integer(0)")), 
    is_identical_to(
      structure(c(2L, 0L, 1L), .Dim = 3L, .Dimnames = list(c("integer(0),1", "2,integer(0)", "integer(0),integer(0)")), class = "table")
    )
  )
  
  # sorting
  expect_that(pltabulate(three, levels = c("integer(0),1", "2,integer(0)", "integer(0),integer(0)"), sort = TRUE), 
    is_identical_to(
      structure(0:2, .Dim = 3L, .Dimnames = list(c("2,integer(0)", "integer(0),integer(0)", "integer(0),1")), class = "table")
    )
  )
})
