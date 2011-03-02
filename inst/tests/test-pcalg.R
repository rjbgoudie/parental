context("pcalg tools")

test_that("toAdjacencyMatrix", {
  sink(tempfile())
  require(pcalg)
  sink()
  
  if (R.version$os == "darwin9.8.0"){
    testfile <- file.path("", "Volumes", "Buster", "library",
                           "parental", "inst", "test-data", 
                           "pc-boot-1.RData")
  } else {
    testfile <- file.path("~", "library",
                             "parental", "inst", "test-data", 
                             "pc-boot-1.RData")
  }
  load(testfile)
  expect_that(
    as.parental(pc.fit), 
    is_identical_to(
      structure(
               list(integer(0), 1L, c(1L, 2L, 4L, 5L), c(2L, 5L), integer(0)),
               class = "parental"))
  )
  
})


test_that("pdag", {
  
  # test v structure
  expect_that(
    as.cpdag0(bn(integer(0), c(1L, 3L), integer(0))),
    is_identical_to(
      structure(list(integer(0), c(1L, 3L), integer(0)), class = "parental")
    )
  )
  
  # test straight line
  expect_that(
    as.cpdag0(bn(integer(0), 1, 2)),
    is_identical_to(
      structure(list(2L, c(1L, 3L), 2L), class = "parental")
    )
  )
  
})

test_that("as.cpdag0", {
  expect_that(as.cpdag0(bn(integer(0))),
              is_identical_to(parental(integer(0))))

  expect_that(as.cpdag0(bn(integer(0), integer(0))),
              is_identical_to(parental(integer(0), integer(0))))
  
  expect_that(as.cpdag0(bn(integer(0), 1L)),
              is_identical_to(parental(2L, 1L)))

  expect_that(as.cpdag0(bn(2L, integer(0))),
              is_identical_to(parental(2L, 1L)))
  
  # next, the basic quadruple of 3-node graphs
  # see, e.g. p256
  # Verma and Pearl. Equivalence and Synthesis of Causal Models.
  # Uncertainty in Artificial Intelligence 6 (1990)
  expect_that(as.cpdag0(bn(integer(0), c(1L, 3L), integer(0))),
              is_identical_to(parental(integer(0), c(1L, 3L), integer(0))))
  
  expect_that(as.cpdag0(bn(integer(0), 1L, 2L)),
              is_identical_to(parental(2L, c(1L, 3L), 2L)))
  
  expect_that(as.cpdag0(bn(2L, integer(0), 2L)),
              is_identical_to(parental(2L, c(1L, 3L), 2L)))
  
  expect_that(as.cpdag0(bn(2L, 3L, integer(0))),
              is_identical_to(parental(2L, c(1L, 3L), 2L)))
  
  # a more unusual example
  expect_that(as.cpdag0(bn(integer(0), c(1L, 3L), 1L)),
              is_identical_to(parental(c(2L, 3L), c(1L, 3L), c(1L, 2L))))
  
  # a 4-node example
  expect_that(as.cpdag0(bn(integer(0), integer(0), 1L, c(1L, 2L))),
              is_identical_to(parental(3L, integer(0), 1L, c(1L, 2L))))
  
  # 5-node example from p260
  # Verma and Pearl. Equivalence and Synthesis of Causal Models.
  # Uncertainty in Artificial Intelligence 6 (1990)
  expect_that(as.cpdag0(bn(integer(0), 1L, 1L, c(2L, 3L), 4L)),
              is_identical_to(parental(c(2L, 3L), 1L, 1L, c(2L, 3L), 4L)))
  
  expect_that(as.cpdag0(bn(2L, integer(0), 1L, c(2L, 3L), 4L)),
              is_identical_to(parental(c(2L, 3L), 1L, 1L, c(2L, 3L), 4L)))
  
  expect_that(as.cpdag0(bn(3L, 1L, integer(0), c(2L, 3L), 4L)),
              is_identical_to(parental(c(2L, 3L), 1L, 1L, c(2L, 3L), 4L)))
})

test_that("as.cpdag0.bnpostmcmc.list", {
  # this test case comes from struct-dag-inf2 test suite
  testmpostl <- structure(list(
    structure(list(samples = structure(list(structure(list(
    2L, integer(0)), class = c("bn", "parental")), structure(list(
    integer(0), 1L), class = c("bn", "parental")), structure(list(
    2L, integer(0)), class = c("bn", "parental")), structure(list(
    integer(0), integer(0)), class = c("bn", "parental")), structure(list(
    integer(0), integer(0)), class = c("bn", "parental"))), 
    class = c("mcmcbn", "bn.list", "parental.list")), 
    tabulated = structure(c(1L, 2L, 2L), .Dim = 3L, 
    .Dimnames = list(c("integer(0),1", "2,integer(0)", 
    "integer(0),integer(0)")), class = "table"), data = structure(list(
    x1 = structure(c(2L, 2L, 1L, 2L), .Label = c("0", "1"), class = "factor"), 
    x2 = structure(c(1L, 2L, 1L, 2L), .Label = c("0", "1"), 
    class = "factor")), .Names = c("x1", "x2"), row.names = c(NA, -4L), 
    class = "data.frame")), .Names = c("samples", "tabulated", "data"), 
    class = "bnpostmcmc"), structure(list(samples = structure(list(
    structure(list(integer(0), 1L), class = c("bn", "parental"
    )), structure(list(integer(0), integer(0)), class = c("bn", 
    "parental")), structure(list(integer(0), 1L), class = c("bn", 
    "parental")), structure(list(2L, integer(0)), class = c("bn", 
    "parental")), structure(list(integer(0), 1L), class = c("bn", 
    "parental"))), class = c("mcmcbn", "bn.list", "parental.list")), 
    tabulated = structure(c(1L, 1L, 3L), .Dim = 3L, .Dimnames = list(
    c("2,integer(0)", "integer(0),integer(0)", "integer(0),1"
    )), class = "table"), data = structure(list(x1 = 
    structure(c(2L, 2L, 1L, 2L), .Label = c("0", "1"), class = "factor"), 
    x2 = structure(c(1L, 2L, 1L, 2L), .Label = c("0", "1"), 
    class = "factor")), .Names = c("x1", "x2"), row.names = c(NA, -4L), 
    class = "data.frame")), .Names = c("samples", "tabulated", "data"), 
    class = "bnpostmcmc")), class = "bnpostmcmc.list")
  
  expected <- list(parental.list(parental(2L, 1L),
                        parental(2L, 1L),
                        parental(2L, 1L),
                        parental(integer(0), integer(0)),
                        parental(integer(0), integer(0))),
                   parental.list(parental(2L, 1L),
                        parental(integer(0), integer(0)),
                        parental(2L, 1L),
                        parental(2L, 1L),
                        parental(2L, 1L)))

  expect_that(as.cpdag0(testmpostl), is_identical_to(expected))
})


test_that("CPDAG <-> DAG conversion", {
  data(discreteData)
  dat <- dat[1:500, ]
  p <- ncol(dat)
  ## define independence test (G^2 statistics)
  indepTest <- disCItest
  ## define sufficient statistics
  suffStat <- list(dm = dat, nlev = c(3, 2, 3, 4, 2), adaptDF = FALSE)
  ## estimate CPDAG
  alpha <- 0.01
  sink(tempfile())
  pc.fit <- pc(suffStat, indepTest, p, alpha, verbose = TRUE)
  sink()
  direct <- as.parental(pc.fit@graph)
  expect_that(direct, is_identical_to(as.cpdag0(as.bn(pc.fit))))
  
  # the following is a fairly weak test
  data(discreteData)
  p <- ncol(dat)
  ## define independence test (G^2 statistics)
  indepTest <- disCItest
  ## define sufficient statistics
  suffStat <- list(dm = dat, nlev = c(3, 2, 3, 4, 2), adaptDF = FALSE)
  ## estimate CPDAG
  alpha <- 0.01
  sink(tempfile())
  pc.fit <- pc(suffStat, indepTest, p, alpha, verbose = TRUE)
  sink()
  direct <- as.parental(pc.fit@graph)
  expect_that(direct, is_identical_to(as.cpdag0(as.bn(pc.fit))))
})

test_that("pdag2", {
  
  # test v structure
  expect_that(
    as.cpdag(bn(integer(0), c(1L, 3L), integer(0))),
    is_identical_to(
      structure(list(integer(0), c(1L, 3L), integer(0)), class = "parental")
    )
  )
  
  # test straight line
  expect_that(
    as.cpdag(bn(integer(0), 1, 2)),
    is_identical_to(
      structure(list(2L, c(1L, 3L), 2L), class = "parental")
    )
  )
  
})

test_that("as.cpdag", {
  expect_that(as.cpdag(bn(integer(0))),
              is_identical_to(parental(integer(0))))

  expect_that(as.cpdag(bn(integer(0), integer(0))),
              is_identical_to(parental(integer(0), integer(0))))
  
  expect_that(as.cpdag(bn(integer(0), 1L)),
              is_identical_to(parental(2L, 1L)))

  expect_that(as.cpdag(bn(2L, integer(0))),
              is_identical_to(parental(2L, 1L)))
  
  # next, the basic quadruple of 3-node graphs
  # see, e.g. p256
  # Verma and Pearl. Equivalence and Synthesis of Causal Models.
  # Uncertainty in Artificial Intelligence 6 (1990)
  expect_that(as.cpdag(bn(integer(0), c(1L, 3L), integer(0))),
              is_identical_to(parental(integer(0), c(1L, 3L), integer(0))))
  
  expect_that(as.cpdag(bn(integer(0), 1L, 2L)),
              is_identical_to(parental(2L, c(1L, 3L), 2L)))
  
  expect_that(as.cpdag(bn(2L, integer(0), 2L)),
              is_identical_to(parental(2L, c(1L, 3L), 2L)))
  
  expect_that(as.cpdag(bn(2L, 3L, integer(0))),
              is_identical_to(parental(2L, c(1L, 3L), 2L)))
  
  # a more unusual example
  expect_that(as.cpdag(bn(integer(0), c(1L, 3L), 1L)),
              is_identical_to(parental(c(2L, 3L), c(1L, 3L), c(1L, 2L))))
  
  # a 4-node example
  expect_that(as.cpdag(bn(integer(0), integer(0), 1L, c(1L, 2L))),
              is_identical_to(parental(3L, integer(0), 1L, c(1L, 2L))))
  
  # 5-node example from p260
  # Verma and Pearl. Equivalence and Synthesis of Causal Models.
  # Uncertainty in Artificial Intelligence 6 (1990)
  expect_that(as.cpdag(bn(integer(0), 1L, 1L, c(2L, 3L), 4L)),
              is_identical_to(parental(c(2L, 3L), 1L, 1L, c(2L, 3L), 4L)))
  
  expect_that(as.cpdag(bn(2L, integer(0), 1L, c(2L, 3L), 4L)),
              is_identical_to(parental(c(2L, 3L), 1L, 1L, c(2L, 3L), 4L)))
  
  expect_that(as.cpdag(bn(3L, 1L, integer(0), c(2L, 3L), 4L)),
              is_identical_to(parental(c(2L, 3L), 1L, 1L, c(2L, 3L), 4L)))
})

test_that("whichVStructure", {
  expect_that(whichVStructure(bn(integer(0))),
              is_identical_to(list(integer(0))))
  
  expect_that(whichVStructure(bn(integer(0), c(1L, 3L), integer(0))),
              is_identical_to(list(integer(0), c(1L, 3L), integer(0))))
})


test_that("as.cpdag.bnpostmcmc.list", {
  # this test case comes from struct-dag-inf2 test suite
  testmpostl <- structure(list(
    structure(list(samples = structure(list(structure(list(
    2L, integer(0)), class = c("bn", "parental")), structure(list(
    integer(0), 1L), class = c("bn", "parental")), structure(list(
    2L, integer(0)), class = c("bn", "parental")), structure(list(
    integer(0), integer(0)), class = c("bn", "parental")), structure(list(
    integer(0), integer(0)), class = c("bn", "parental"))), 
    class = c("mcmcbn", "bn.list", "parental.list")), 
    tabulated = structure(c(1L, 2L, 2L), .Dim = 3L, 
    .Dimnames = list(c("integer(0),1", "2,integer(0)", 
    "integer(0),integer(0)")), class = "table"), data = structure(list(
    x1 = structure(c(2L, 2L, 1L, 2L), .Label = c("0", "1"), class = "factor"), 
    x2 = structure(c(1L, 2L, 1L, 2L), .Label = c("0", "1"), 
    class = "factor")), .Names = c("x1", "x2"), row.names = c(NA, -4L), 
    class = "data.frame")), .Names = c("samples", "tabulated", "data"), 
    class = "bnpostmcmc"), structure(list(samples = structure(list(
    structure(list(integer(0), 1L), class = c("bn", "parental"
    )), structure(list(integer(0), integer(0)), class = c("bn", 
    "parental")), structure(list(integer(0), 1L), class = c("bn", 
    "parental")), structure(list(2L, integer(0)), class = c("bn", 
    "parental")), structure(list(integer(0), 1L), class = c("bn", 
    "parental"))), class = c("mcmcbn", "bn.list", "parental.list")), 
    tabulated = structure(c(1L, 1L, 3L), .Dim = 3L, .Dimnames = list(
    c("2,integer(0)", "integer(0),integer(0)", "integer(0),1"
    )), class = "table"), data = structure(list(x1 = 
    structure(c(2L, 2L, 1L, 2L), .Label = c("0", "1"), class = "factor"), 
    x2 = structure(c(1L, 2L, 1L, 2L), .Label = c("0", "1"), 
    class = "factor")), .Names = c("x1", "x2"), row.names = c(NA, -4L), 
    class = "data.frame")), .Names = c("samples", "tabulated", "data"), 
    class = "bnpostmcmc")), class = "bnpostmcmc.list")
  
  expected <- list(parental.list(parental(2L, 1L),
                        parental(2L, 1L),
                        parental(2L, 1L),
                        parental(integer(0), integer(0)),
                        parental(integer(0), integer(0))),
                   parental.list(parental(2L, 1L),
                        parental(integer(0), integer(0)),
                        parental(2L, 1L),
                        parental(2L, 1L),
                        parental(2L, 1L)))

  expect_that(as.cpdag(testmpostl), is_identical_to(expected))
})

test_that("CPDAG <-> DAG conversion as.cpdag", {
  data(discreteData)
  dat <- dat[1:500, ]
  p <- ncol(dat)
  ## define independence test (G^2 statistics)
  indepTest <- disCItest
  ## define sufficient statistics
  suffStat <- list(dm = dat, nlev = c(3, 2, 3, 4, 2), adaptDF = FALSE)
  ## estimate CPDAG
  alpha <- 0.01
  sink(tempfile())
  pc.fit <- pc(suffStat, indepTest, p, alpha, verbose = TRUE)
  sink()
  direct <- as.parental(pc.fit@graph)
  expect_that(direct, is_identical_to(as.cpdag(as.bn(pc.fit))))
  
  # the following is a fairly weak test
  data(discreteData)
  p <- ncol(dat)
  ## define independence test (G^2 statistics)
  indepTest <- disCItest
  ## define sufficient statistics
  suffStat <- list(dm = dat, nlev = c(3, 2, 3, 4, 2), adaptDF = FALSE)
  ## estimate CPDAG
  alpha <- 0.01
  sink(tempfile())
  pc.fit <- pc(suffStat, indepTest, p, alpha, verbose = TRUE)
  sink()
  direct <- as.parental(pc.fit@graph)
  expect_that(direct, is_identical_to(as.cpdag(as.bn(pc.fit))))
})
