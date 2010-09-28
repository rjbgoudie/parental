context("Plotting")

test_that("Basic plots", {
  controlfn <- function(){
    # run this function to generate the controls
    # first cd into parental/tests
    # then run this function
    currentwd <- getwd()
    setwd("/Volumes/Buster 1/library/parental/tests")
    library(graphicsQC)
    set.seed(580)
    plotcontrol <- plotExpr(
      c(
        "print(grplot(parental(integer(0), 1L)))",
        "print(grplot(parental(integer(0), c(1L, 2L), integer(0))))",
        "print(grplot(parental(integer(0), integer(0), 1L)))",
        "print(grplot(parental(integer(0), integer(0), 2L)))"
      ),
      path = "../../test-data/parental-plot-control",
      clear = TRUE,
      filetype = "png",
      prefix = "control"
    )
    setwd(currentwd)
  }
  
  library(graphicsQC)
  
  # generate test data
  set.seed(580) 
  plottest <- plotExpr(
    c(
      "print(grplot(parental(integer(0), 1L)))",
      "print(grplot(parental(integer(0), c(1L, 2L), integer(0))))",
      "print(grplot(parental(integer(0), integer(0), 1)))",
      "print(grplot(parental(integer(0), integer(0), 2)))"
      # need to add more expectations below
    ),
    path = "../../test-data/parental-plot-test",
    clear = TRUE,
    filetype = "png",
    prefix = "test"
  )
  
  # compare test data to the controls
  sink(tempfile())
  res <- compare(
    test = plottest,
    control = "../../test-data/parental-plot-control"
  )
  sink()
  
  # check that tests and controls are identical
  expect_that(res$results$png[[1]]$result, is_identical_to("identical"))
  expect_that(res$results$png[[2]]$result, is_identical_to("identical"))
  expect_that(res$results$png[[3]]$result, is_identical_to("identical"))
  expect_that(res$results$png[[4]]$result, is_identical_to("identical"))
})
