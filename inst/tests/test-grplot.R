context("Plotting primitives")

test_that("toAdjacencyMatrix", {
  plot <- F
  
  x1 <- 0
  y1 <- 0
  x2 <- 1
  y2 <- 1
  w1 <- 0.25
  h1 <- 0.25
  w2 <- 0.25
  h2 <- 0.25
  
  if (plot){
    plot(x = c(x1, x2),
           y = c(y1, y2))
    lines(x = c(x1 - w1/2, x1 + w1/2, x1 + w1/2, x1 - w1/2, x1 - w1/2),
          y = c(y1 - h1/2, y1 - h1/2, y1 + h1/2, y1 + h1/2, y1 - h1/2))
    lines(x = c(x2 - w2/2, x2 + w2/2, x2 + w2/2, x2 - w2/2, x2 - w2/2),
          y = c(y2 - h2/2, y2 - h2/2, y2 + h2/2, y2 + h2/2, y2 - h2/2))
    out <- coordinatesBetweenRectangles(x1, y1, x2, y2, w1, h1, w2, h2)
    lines(x = out[c(1,3)],
          y = out[c(2,4)])
  }
  
  expect_that(
    coordinatesBetweenRectangles(x1, y1, x2, y2, w1, h1, w2, h2),
    equals(c(0.125, 0.125, 0.875, 0.875)))
  
  x1 <- 1
  y1 <- 1
  x2 <- 0
  y2 <- 0
  w1 <- 0.25
  h1 <- 0.25
  w2 <- 0.25
  h2 <- 0.25
  expect_that(
    coordinatesBetweenRectangles(x1, y1, x2, y2, w1, h1, w2, h2),
    equals(c(0.875, 0.875, 0.125, 0.125)))

  x1 <- 1
  y1 <- 1
  x2 <- 1
  y2 <- 0
  w1 <- 0.25
  h1 <- 0.25
  w2 <- 0.25
  h2 <- 0.25
  
  x1 <- 1
  y1 <- 0
  x2 <- 0
  y2 <- 0
  w1 <- 0.25
  h1 <- 0.25
  w2 <- 0.25
  h2 <- 0.25
  
  x1 <- 1
  y1 <- 1
  x2 <- 0
  y2 <- 0
  w1 <- 0.25
  h1 <- 0.25
  w2 <- 0.25
  h2 <- 0.25
  
  x1 <- 1.5
  y1 <- 2
  x2 <- 0
  y2 <- 0
  w1 <- 0.5
  h1 <- 0.5
  w2 <- 0.25
  h2 <- 0.25
})