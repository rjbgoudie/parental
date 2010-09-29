context("Sample")

test_that("simulate-sanity", {

  cptable1 <- as.table(array(c(0.25, 0.75, 0.75, 0.25), 2, 2))
  cptable2 <- as.table(array(c(0.5, 0.5)))
  cptable <- list(cptable1, cptable2)
  
  expect_that(simulate.bn(bn      = bn(1, NULL),
                          ptables = cptable,
                          N       = 10),
              throws_error())
})


test_that("simulate.bn (Two node)", {
  cpt <- list(
    as.table(array(c(0.7, 0.3), 2)), 
    as.table(array(c(0.5, 0.5, 0.2, 0.8), c(2, 2)))
  )
  net <- bn(NULL, 1)
  sim <- simulate.bn(net, cpt, 1000)
  
  col1 <- as.vector(table(sim[, 1]))
  col2 <- as.vector(table(sim[, 2]))
  
  expect_that(col1[1], is_within(700, 50))
  expect_that(col2[1], is_within(410, 50))
})

test_that("simulate.bn (Three node)", {
  cpt <- list(
    as.table(array(c(0.7, 0.3), 2)), 
    as.table(array(c(0.5, 0.5,
                     0.2, 0.8),
                   c(2, 2))),
    as.table(array(c(
                # prob of 1 then 2 given
      0.8, 0.2, # p1 = 1, p2 = 1
      0.2, 0.8, # p1 = 2, p2 = 1
      0.2, 0.8, # p1 = 1, p2 = 2
      0.2, 0.8  # p1 = 2, p2 = 2
    ), c(2, 2, 2)))
  )
  net <- bn(NULL, 1L, c(1L, 2L))
  sim <- simulate.bn(net, cpt, 1000)
  
  col1 <- as.vector(table(sim[, 1]))
  col2 <- as.vector(table(sim[, 2]))
  col3 <- as.vector(table(sim[, 3]))
  
  expect_that(col1[1], is_within(700, 30))
  expect_that(col2[1], is_within(410, 30)) # P(col2 == 1)
                                           # = 0.7 * 0.5 + 0.3 * 0.2
  expect_that(col3[1], is_within(410, 30)) # P(col2 == 1)
                                           # = 0.7 * 0.5 * 0.8 + 
                                           #   0.3 * 0.2 * 0.2 + 
                                           #   0.7 * 0.5 * 0.2 + 
                                           #   0.3 * 0.8 * 0.2
})

test_that("simulate.bn (Three node)", {
  cpt <- list(
    as.table(array(c(0.7, 0.3), 2)), 
    as.table(array(c(0.5, 0.5,
                     0.2, 0.8),
                   c(2, 2))),
    as.table(array(c(
                # prob of 1 then 2 given
      0.99, 0.01, # p1 = 1, p2 = 1
      0.99, 0.01, # p1 = 2, p2 = 1
      0.99, 0.01, # p1 = 1, p2 = 2
      0.99, 0.01  # p1 = 2, p2 = 2
    ), c(2, 2, 2)))
  )
  net <- bn(NULL, 1L, c(1L, 2L))
  sim <- simulate.bn(net, cpt, 1000)
  
  col1 <- as.vector(table(sim[, 1]))
  col2 <- as.vector(table(sim[, 2]))
  col3 <- as.vector(table(sim[, 3]))
  
  expect_that(col1[1], is_within(700, 30))
  expect_that(col2[1], is_within(410, 30)) # P(col2 == 1)
                                           # = 0.7 * 0.5 + 0.3 * 0.2
  expect_that(col3[1], is_within(999, 30)) # P(col2 == 1)
                                           # = 0.7 * 0.5 * 0.8 + 
                                           #   0.3 * 0.2 * 0.2 + 
                                           #   0.7 * 0.5 * 0.2 + 
                                           #   0.3 * 0.8 * 0.2
})

test_that("simulate.bn (Four node)", {
  set.seed(1261)
  
  net <- bn(4, integer(0), c(1, 2), integer(0))

  node4 <- as.table(array(c(
       0.81, # prob of 1
       0.19  # prob of 2
     ), 2))

  node2 <- as.table(array(c(
       0.4886731, # prob of 1
       1-0.4886731  # prob of 2
     ), 2))

  node1 <- as.table(array(c(
             # prob of 1 then 2 given
   0.0005, 1-0.0005, # p = 1
   0, 1  # p = 2
  ), c(2, 2)))

  node3 <- as.table(array(c(
             # prob of 1 then 2 given
   1, 0, # p1 = 1, p2 = 1
   0.265, 1-0.265, # p1 = 2, p2 = 1
   0.5, 0.5, # p1 = 1, p2 = 2
   1-0.7626582, 0.7626582  # p1 = 2, p2 = 2
  ), c(2, 2, 2)))

  cpt <- list(node1, node2, node3, node4)
  N <- 10000
  sim <- simulate.bn(net, cpt, N = N)
  
  col1 <- as.vector(table(sim[, 1]))
  col2 <- as.vector(table(sim[, 2]))
  col3 <- as.vector(table(sim[, 3]))
  col4 <- as.vector(table(sim[, 4]))
  
  expect_that(col4[1], is_within(0.81 * N, 45))
  
  p2 <- 0.4886731
  sd2 <- sqrt(N * p2 * (1 - p2))
  expect_that(col2[1], is_within(p2 * N, sd2))
  
  PX1equals0 <- 0.0005 * 0.81 + 0 * 0.19
  PX1equals1 <- (1-0.0005) * 0.81 + 1 * 0.19
  
  sd1 <- sqrt(N * PX1equals1 * (1 - PX1equals1))
  expect_that(col1[1], is_within(PX1equals0 * N, sd1))
  
  PX2equals0 <- 0.4886731
  PX2equals1 <- 1 - PX2equals0
  
  PX1equals0andX2equals0 <- PX1equals0 * PX2equals0
  PX1equals0andX2equals1 <- PX1equals0 * PX2equals1
  PX1equals1andX2equals0 <- PX1equals1 * PX2equals0
  PX1equals1andX2equals1 <- PX1equals1 * PX2equals1
  
  nX1equals0andX2equals0 <- nrow(subset(sim, V1 == "1" & V2 == "1"))
  sd00 <- 2 * sqrt(N* PX1equals0andX2equals0 * (1 - PX1equals0andX2equals0))
  expect_that(nX1equals0andX2equals0, is_within(PX1equals0andX2equals0 * N, sd00))
  
  nX1equals0andX2equals1 <- nrow(subset(sim, V1 == "1" & V2 == "2"))
  sd01 <- 2 * sqrt(N* PX1equals0andX2equals1 * (1 - PX1equals0andX2equals1))
  expect_that(nX1equals0andX2equals1, is_within(PX1equals0andX2equals1 * N, sd01))
  
  nX1equals1andX2equals0 <- nrow(subset(sim, V1 == "2" & V2 == "1"))
  sd10 <- sqrt(N* PX1equals1andX2equals0 * (1 - PX1equals1andX2equals0))
  expect_that(nX1equals1andX2equals0, is_within(PX1equals1andX2equals0 * N, sd10))
  
  nX1equals1andX2equals1 <- nrow(subset(sim, V1 == "2" & V2 == "2"))
  sd11 <- sqrt(N* PX1equals1andX2equals1 * (1 - PX1equals1andX2equals1))
  expect_that(nX1equals1andX2equals1, is_within(PX1equals1andX2equals1 * N, sd11))
  
  PX3equals0 <- 1 * PX1equals0andX2equals0 + 
                0.5 * PX1equals0andX2equals1 + 
                0.265 * PX1equals1andX2equals0 + 
                (1 - 0.7626582) * PX1equals1andX2equals1
  
  sum(c(PX1equals0andX2equals0,
  PX1equals0andX2equals1,
  PX1equals1andX2equals0,
  PX1equals1andX2equals1))
  
  sd3 <- sqrt(N * PX3equals0 * (1 - PX3equals0))
  expect_that(col3[1], is_within(PX3equals0 * N, sd3))
})

test_that("simulate.bn (Errors)", {
  cpt <- list(
    as.table(array(c(0.7, 0.3), 2)), 
    as.table(array(c(0.5, 0.5, 0.2, 0.8), c(2, 2)))
  )
  
  # cyclic input
  net <- bn(2, 1)
  expect_that(simulate.bn(net, cpt, 1000), throws_error())
})
