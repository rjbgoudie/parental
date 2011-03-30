# Part of the "parental" package, http://github.com/rjbgoudie/parental
# 
# This software is distributed under the GPL-3 license.  It is free,
# open source, and has the attribution requirements (GPL Section 7) in
#   http://github.com/rjbgoudie/parental
# 
# Note that it is required that attributions are retained with each function.
#
# Copyright 2008 Robert J. B. Goudie, University of Warwick

# context("Plotting")
# 
# test_that("Basic plots", {
#   controlfn <- function(){
#     # run this function to generate the controls
#     # first cd into parental/tests
#     # then run this function
#     currentwd <- getwd()
#     setwd("/Volumes/Buster 1/library/parental/tests")
#     library(graphicsQC)
#     set.seed(580)
#     plotcontrol <- plotExpr(
#       c(
#         "print(grplot(parental(integer(0), 1L)))",
#         "print(grplot(parental(integer(0), c(1L, 2L), integer(0))))",
#         "print(grplot(parental(integer(0), integer(0), 1L)))",
#         "print(grplot(parental(integer(0), integer(0), 2L)))"
#       ),
#       path = "../../test-data/parental-plot-control",
#       clear = TRUE,
#       filetype = "png",
#       prefix = "control"
#     )
#     setwd(currentwd)
#   }
#   
#   if (require(lattice) & require(network) & require(graphicsQC) & R.version$os == "darwin9.8.0"){
#   
#     # generate test data
#     set.seed(580)
#     testfile <- file.path("", "Volumes", "Buster", "library",
#                              "parental", "tests", "test-data", 
#                              "parental-plot-test")
#       
#     controlfile <- file.path("", "Volumes", "Buster", "library",
#                              "parental", "tests", "test-data", 
#                              "parental-plot-control")
#                              
#     cat("hihihihihihi")
#     cat(getwd())
#     cat(file.access(testfile, mode = 0))
#     cat(file.access(controlfile, mode = 0))
#     cat("hihihihihihi")
#     # 
#     # testfile <- file.path("..", "tests", "test-data", 
#     #                          "parental-plot-test")
#     #   
#     # controlfile <- file.path("..", "tests", "test-data", 
#     #                          "parental-plot-control")
#     # 
#     # cat("hihihihihihi")
#     # cat(getwd())
#     # cat(file.access(testfile, mode = 0))
#     # cat(file.access(controlfile, mode = 0))
#     # cat("hihihihihihi")
#     # 
#     # testfile <- "test-data/parental-plot-test/"
#     # controlfile <- "test-data/parental-plot-control"
#     # 
#     # cat("hihihihihihi")
#     # cat(getwd())
#     # cat(file.access(testfile, mode = 0))
#     # cat(file.access(controlfile, mode = 0))
#     # cat("hihihihihihi")
#     # 
#     # testfile <- c(system.file("tests", "test-data", "parental-plot-test", 
#     #                           package = "parental"))
#     # 
#     # controlfile <- c(system.file("tests", "test-data", "parental-plot-control", 
#     #                           package = "parental"))
#     # 
#     # cat("hihihihihihi")
#     # cat(getwd())
#     # cat(file.access(testfile, mode = 0))
#     # cat(file.access(controlfile, mode = 0))
#     # cat("hihihihihihi")
#     # 
#     plottest <- plotExpr(
#       c(
#         "print(grplot(parental(integer(0), 1L)))",
#         "print(grplot(parental(integer(0), c(1L, 2L), integer(0))))",
#         "print(grplot(parental(integer(0), integer(0), 1)))",
#         "print(grplot(parental(integer(0), integer(0), 2)))"
#         # need to add more expectations below
#       ),
#       path = testfile,
#       clear = TRUE,
#       filetype = "png"
#     )
#   
#     # compare test data to the controls
#     sink(tempfile())
#     res <- compare(
#       test = plottest,
#       control = controlfile
#     )
#     sink()
#   
#     # check that tests and controls are identical
#     expect_that(res$results$png[[1]]$result, is_identical_to("identical"))
#     expect_that(res$results$png[[2]]$result, is_identical_to("identical"))
#     expect_that(res$results$png[[3]]$result, is_identical_to("identical"))
#     expect_that(res$results$png[[4]]$result, is_identical_to("identical"))
#   }
# })
