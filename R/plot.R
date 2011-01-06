

#' Convert cartesian coordinates to polars
#'
#' Currently unused. Not sure if this is vectorised
#'
#' @param x Cartesian x-coordinates
#' @param y Cartesian y-coordinates
#' @return A list with two components, named \code{r} and \code{theta}, 
#'   containing the radius and angle to the supplied point.
toPolar <- function(x, y){
  r <- sqrt(x^2 + y^2)
  theta <- rep(NULL, times = length(r))
  theta[x >= 0] <- asin(y[x >= 0]/r[x >= 0])
  theta[x < 0] <- -asin(y[x < 0]/r[x < 0]) + pi
  theta[is.null(theta)] <- 0
  list(r = r, theta = theta)
}

#' Convert polar coordinates to Cartesian
#'
#' Currently unused. Not sure if this is vectorised
#'
#' @param r Polar coordinates
#' @param theta Polar coordinates
#' @return A list with two components, named \code{x} and \code{y}, 
#'   containing the coordinates of the supplied point.
toCartesian <- function(r, theta){
  list(x = r * cos(theta), y = r * sin(theta))
}

#' Calculate distance between two points
#'
#' A vectorised function for calculating the distance between two 
#' points in Cartesian space. Computes the distance between \code{(x[1], 
#' y[1])} and \code{(x[2], y[2])}.
#'
#' @param x A numeric vector of length 2, containing the x-coordinates of 
#'   the two points
#' @param y A numeric vector of length 2, containing the y-coordinates of 
#'   the two points
#' @return A numeric vector of length 1. The distance.
distance <- function(x, y){
  sqrt((y[1] - x[1])^2 + (y[2] - x[2])^2)
}

#' Calculate coordinates of line between bounding boxes
#'
#' We have points \code{(x1, y1)} and \code{(x2, y2)} in Cartesian space. 
#' We wish to draw a line between these two points.
#' 
#' However, these two points have rectangles centred around them.
#' The first point has a rectangle width \code{w1} and height \code{h1} drawn 
#' around it; the second has a rectangle with \code{w2} and height \code{h2}
#' drawn around it. Note that the vertices of the first rectangle are 
#' \code{(x1-w1/2, x1+h1/2)}, \code{(x1+w1/2, x1+h1/2)},
#' \code{(x1+w1/2, x1-h1/2)} \code{(x1-w1/2, x1-h1/2)}.
#' 
#' This function returns the coordinates of the points on which the 
#' line drawn between the two points intersects with each bounding box.
#' 
#' @param x1 The x-coordinate of the first point
#' @param y1 The y-coordinate of the first point
#' @param x2 The x-coordinate of the second point
#' @param y2 The y-coordinate of the second point
#' @param w1 The width of the bounding box around \code{(x1, y1)}
#' @param h1 The height of the bounding box around \code{(x1, y1)}
#' @param w2 The width of the bounding box around \code{(x2, y2)}
#' @param h2 The height of the bounding box around \code{(x1, y1)}
#' @return A numeric vector of length 4. The first two are the x- and 
#'   y-coordinates on the bounding box of \code{(x1, y1)}. The 
#'   third and fourth are the x- and y-coordinates on the bounding box of 
#'   \code{(x2, y2)}
coordinatesBetweenRectangles <- function(x1, y1, x2, y2, w1, h1, w2, h2){
  switchBack <- F
  if (x1 > x2){
    switchBack <- T
    s1 <- x1
    t1 <- y1
    u1 <- w1
    v1 <- h1
    
    x1 <- x2
    y1 <- y2
    w1 <- w2
    h1 <- h2
    
    x2 <- s1
    y2 <- t1
    w2 <- u1
    h2 <- v1
  }
  
  s <- (y2 - y1)/(x2 - x1)
  if (s > h1/w1){
    # so top edge
    # top edge y = y1 + h1/2
    ox1 <- h1/2 * (x2 - x1)/(y2 - y1) + x1
    oy1 <- y1 + h1/2
  }
  else if (-h1/w1 <= s && s <= h1/w1){
    # so side edge
    # side edge x = x1 + w1/2
    ox1 <- x1 + w1/2
    oy1 <- y1 + (y2 - y1)/(x2 - x1) * w1/2
  }
  else if (s < h1/w1){
    # so bottom edge
    # bottom edge y = y1 - h1/2
    ox1 <- (-h1/2) * (x2 - x1)/(y2 - y1) + x1
    oy1 <- y1 - h1/2
  }
  else {
    stop("Error")
  }

  if (s > h2/w2){
    # so bottom edge
    # bottom edge y = y2 - h2/2
    ox2 <- (y2 - h2/2 - y1) * (x2 - x1)/(y2 - y1) + x1
    oy2 <- y2 - h2/2
  }
  else if (-h2/w2 <= s && s <= h2/w2){
    # so side edge
    # side edge x = x2 - w2/2
    ox2 <- x2 - w2/2
    oy2 <- y1 + (y2 - y1)/(x2 - x1) * (x2 - w2/2 - x1)
  }
  else if (s < h2/w2){
    # so top edge
    # top edge y = y2 + h2/2
    ox2 <- (y2 + h2/2 - y1) * (x2 - x1)/(y2 - y1) + x1
    oy2 <- y2 + h2/2
  }
  else {
    stop("Error")
  }
  
  if (switchBack){
    tox1 <- ox1
    toy1 <- oy1
    
    ox1 <- ox2
    oy1 <- oy2
    
    ox2 <- tox1
    oy2 <- toy1
  }
  c(ox1, oy1, ox2, oy2)
}

#' Convert a lattice object to a grob
#'
#' See Paul Murrell's message to r-help - Mar 21, 2010 8:21:39 pm.
#' "Re: [R] lattice grob"
#' http://markmail.org/message/tg6kdxsr74fynncy
#'
#' @param p A lattice object
#' @param ... further arguments
#' @return A grob
latticeGrob <- function(p, ...){
   grob(p = p, ..., cl = "lattice")
}

#' drawDetails for lattice plots
#' 
#' See Paul Murrell's message to r-help - Mar 21, 2010 8:21:39 pm.
#' "Re: [R] lattice grob"
#' http://markmail.org/message/tg6kdxsr74fynncy
#'
#' @param x ....
#' @param recording ...
#' @return ....
drawDetails.lattice <- function(x, recording = F){
   lattice::plot.trellis(x$p, newpage = F)
}

#' Pre-panel function for plotting parental objects
#' 
#' Computes the bounds of the final plot.
#'
#' @param x The x-coordinates of the nodes
#' @param y The y-coordinates of the nodes
#' @param parents An object of class 'parental'
#' @param rawdata A data frame containing the raw data
#' @param grobNodeSize A function that returns the size of a node grob
#' @param offset An offset
#' @param islist A logical
#' @param widthMultiplier A width multiplier
#' @param heightMultipler A height multiplier
#' @return A list of length two. This contains two components: \code{xlim} 
#'   and \code{ylim}, each of which contain the max and min values in 
#'   that dimension.
#' @export
prepanel.parental <- function(x, y, parents, rawdata = NULL, 
                              grobNodeSize, offset, islist = F,
                              widthMultiplier = 2, heightMultipler = 1){
  if (!islist){
    parents <- list(parents)
  }
  out <- sapply(parents, function(parents){
    
    nodeSize <- sapply(seq_along(parents), function(i){
      out <- grobNodeSize(node = i, parents, rawdata)
      # this spawns a new plot, and is probably the wrong approach
      # anyway
      c(width  = out[[1]] + offset, 
           height = out[[2]] + offset)
    })
    totals <- rowSums(nodeSize)
    widths <- nodeSize["width", ] / totals["width"] * 
                max(diff(range(x)), diff(range(y))) * widthMultiplier
    heights <- nodeSize["height", ] / totals["height"] * 
                 max(diff(range(x)), diff(range(y))) * heightMultipler
    c(range(c(x - widths/2, x + widths/2)),
      range(c(y - heights/2, y + heights/2)))
  })
  #browser()
  list(xlim = c(min(unlist(out[1, ])), max(unlist(out[2, ]))),
       ylim = c(min(unlist(out[3, ])), max(unlist(out[4, ]))))
}

#' Expand coordinates
#'
#' Expand the coordinates. Not currently used
#'
#' @param width A vector of widths?
#' @param height A vector of heights?
#' @param x x-coordinates
#' @param y y-coordinates
#' @return A list with two components: \code{width} and \code{height}.
convertToEnlargedCoordinates <- function(width, height, x, y){
  curr.xlim <- current.panel.limits()$xlim
  curr.ylim <- current.panel.limits()$ylim
  list(width = width * (diff(range(x)) / diff(range(curr.xlim))),
       height = height * (diff(range(y)) / diff(range(curr.ylim))))
}


#' Return the size of node "node" when the name of the node is plotted
#' 
#' @param node An integer of length 1, indicating which node the dimensions
#'   should be computed for.
#' @param parents An object of class "parental" containing the graph that 
#'   is to be plotted
#' @param rawdata The rawdata
#' @return A list of length 2 containing two items:
#'     width: A object of class "unit"
#'     height: A object of class "unit"
#' @export
grobNodeNameSize <- function(node, parents, rawdata = NULL){
  #tg <- textGrob(label = names(parents)[node])
  #list(width  = grobWidth(tg),
  #     height = grobHeight(tg))
  list(width  = nchar(names(parents)[node]),
       height = length(gregexpr("\n", names(parents)[node], fixed = T)[[1]]))
}

#' Return a grob for node "node" when the name of the node is plotted
#' 
#' @param node An integer of length 1, indicating which node the dimensions
#'   should be computed for.
#' @param parents An object of class "parental" containing the graph that 
#'   is to be plotted
#' @param rawdata The rawdata
#' @return A "grob"
#' @export
grobNodeName <- function(node, parents, rawdata = NULL){
  textGrob(label = names(parents)[node])
}

#' Return the size of levelplot for node "node". If the node has no 
#' parents, the size of a grobNodeName is returned instead.
#' 
#' @param node An integer of length 1, indicating which node the dimensions
#'   should be computed for.
#' @param parents An object of class "parental" containing the graph that 
#'   is to be plotted.
#' @param rawdata The rawdata
#'            
#' @return A list of length 2 containing two items:
#'     width: A object of class "unit"
#'     height: A object of class "unit"
#' @export
grobNodeLevelPlotSize <- function(node, parents, rawdata){
  nParents <- length(parents[[node]])
  if (nParents > 0){
    nLevels <- nlevels(rawdata[, node])
    nLevelsParents <- sapply(parents[[node]], function(j){
      nlevels(rawdata[, j])
    })
    list(width = (nParents + nLevels),
         height = (prod(nLevelsParents) + 3))
  }
  else {
    grobNodeNameSize(node, parents, rawdata)
  }
}

#' Return the default theme
#'
#' Returns the default theme
#'
#' @return Returns the default theme
#' @export
grobNodeLevelPlotDefaultTheme <- function(){
  list(layout.heights = list(top.padding       = 0,
                             main.key.padding  = 0.5,
                             key.axis.padding  = 0,
                             axis.xlab.padding = 0,
                             xlab.key.padding  = 0,
                             key.sub.padding   = 0,
                             bottom.padding    = 0),
       layout.widths = list(left.padding      = 0,
                            key.ylab.padding  = 0,
                            ylab.axis.padding = 0,
                            axis.key.padding  = 0,
                            right.padding     = 0),
        par.main.text = list(cex = 0.5))
}

#' Return a grob for node "node" when a levelplot is plotted on each node
#' 
#' @param node  An integer of length 1, indicating which node the dimensions
#'   should be computed for.
#' @param parents An object of class "parental" containing the graph that 
#'   is to be plotted
#' @param rawdata The rawdata
#' @param strip.lines ...
#' @param strip.left.lines ...
#' @param theme ...
#' @param horizontal ...
#' @param ... ...
#' @return A "grob"
#' @export
grobNodeLevelPlot <- function(node,
                              parents,
                              rawdata,
                              strip.lines = 20, 
                              strip.left.lines = 15,
                              theme = grobNodeLevelPlotDefaultTheme(),
                              horizontal = F,
                              ...){
  if (length(parents[[node]]) > 0){
    rd <- rawdata[, c(node, parents[[node]])]
    df <- data.frame(.dummy1 = factor(0), .dummy2 = factor(0), rd)
    propSeq <- seq(from = 4, to = length(c(node, parents[[node]])) + 2)
    dft <- as.data.frame(prop.table(table(df), propSeq))
    roundedFreq <- round(dft[, "Freq"], 2)
    countTable <- as.data.frame(table(df))[, "Freq"]
    dft[, "Freq"] <- paste(roundedFreq, " (", countTable, ")")
    
    conds <- paste("`", names(rd), "`", collapse = "+", sep = "")
    form <- eval(as.formula(
      paste("Freq ~ .dummy1 + .dummy2 | ", conds)), 
      dft
    )
    
    col.l <- colorRampPalette(c('white', 'blue'))(30)
    p1 <- levelplot(
      form,
      data = dft,
      xlab = NULL,
      ylab = NULL,
      main = list(label = paste(names(parents)[node], "\nParents: ", 
                          paste(strwrap(paste(
                            rev(names(parents)[parents[[node]]]), 
                            collapse = ", "), width = 60), collapse = ""), 
                            "."), fontsize = 7),
      colorkey = NULL,
      panel = function(x, y, z, subscripts, ...){

        thisseq <- seq(1, 2*length(z), by = 2)
        z2 <- as.numeric(unlist(strsplit(z, " (", fixed = T))[thisseq])
        panel.levelplot(x, y, z2, subscripts, ...)
        ltext(x[1], y[1], z[subscripts], col = "black", cex = 0.3)
      },
      at = do.breaks(c(-0.01, 1.01), 30),
      col.regions = col.l,
      scales = list(draw = F),
      par.strip.text = list(cex = 0.5),
      par.settings = theme
    )
    
    p1 <- useOuterStrips2(p1,
                          strip.lines      = strip.lines,
                          strip.left.lines = strip.left.lines,
                          horizontal = horizontal)
    latticeGrob(p1)
  }
  else {
    grobNodeName(node, parents, rawdata)
  }
}

#' Panel function for ploting a parental graph
#' 
#' Panel function for grplot.
#'
#' @param x The x-coordinates of the nodes
#' @param y The y-coordinates of the nodes
#' @param parents An object of class parental
#' @param layout The layout (not currently used??)
#' @param col A vector of colours for the nodes (not currently used??)
#' @param alpha A vector of alpha values for the nodes (not currently used??)
#' @param edgecol A matrix of edge colours
#' @param edgealpha A matrix of edge alpha values
#' @param islist A logical of length 1
#' @param rawdata A data frame of raw data.
#' @param grobNode A function that returns the grob for a given node
#' @param grobNodeSize A function that returns the size of a grob for a 
#'   given node
#' @param offset An offset
#' @param widthMultiplier A width multiplier
#' @param heightMultipler A height multiplier
#' @param ... Further arguments
#' @return A panel
#' @export
panel.parental <- function(x, y, parents, layout, col, alpha, 
                           edgecol, 
                           edgealpha, islist = F, rawdata = NULL,
                           grobNode,
                           grobNodeSize,
                           offset,
                           widthMultiplier = 2,
                           heightMultipler = 1,
                           ...){
  if (missing(edgecol)){
    edgecol <- standard.theme(color = T)$col[[1]]
  }
  numberOfNodes <- length(parents)
  if (length(edgecol) == 1){
    edgecol <- matrix(edgecol, ncol = numberOfNodes, nrow = numberOfNodes)
  }
  if (missing(edgealpha)){
    edgealpha <- matrix(1, ncol = numberOfNodes, nrow = numberOfNodes)
  }
  if (length(edgealpha) == 1){
    edgealpha <- matrix(edgealpha,
                        ncol = numberOfNodes, nrow = numberOfNodes)
  }
  if (islist){
    parents <- parents[[panel.number()]]
  }
  # compute the node dimensions
  nodeSize <- lapply(seq_along(parents), function(i){
    size <- grobNodeSize(node = i, parents, rawdata)
#    browser()
    size[[1]] <- (size[[1]] + offset) / max(diff(range(x)), diff(range(y)))
    size[[2]] <- (size[[2]] + offset) / max(diff(range(x)), diff(range(y)))
    #size <- convertToEnlargedCoordinates(size[[1]], size[[2]], x, y)
    c(width  = size[[1]], 
      height = size[[2]])
  })
  
  nodeSize <- sapply(seq_along(parents), function(i){
    out <- grobNodeSize(node = i, parents, rawdata)
    # this spawns a new plot, and is probably the wrong approach
    # anyway
    c(width  = out[[1]] + offset, 
         height = out[[2]] + offset)
  })
  
  totals <- rowSums(nodeSize)
  widths <- nodeSize["width", ] / totals["width"] * 
              max(diff(range(x)), diff(range(y))) * widthMultiplier
  heights <- nodeSize["height", ] / totals["height"] * 
               max(diff(range(x)), diff(range(y))) * heightMultipler
  nodeSize <- lapply(seq_along(parents), function(i){
    c(width  = widths[i], 
      height = heights[i])
  })
  
  nodeSizeNoOffset <- lapply(seq_along(parents), function(i){
    size <- grobNodeSize(node = i, parents, rawdata)
    #browser()
    size[[1]] <- (size[[1]]) / max(diff(range(x)), diff(range(y)))
    size[[2]] <- (size[[2]]) / max(diff(range(x)), diff(range(y)))
    #size <- convertToEnlargedCoordinates(size[[1]], size[[2]], x, y)
    c(width  = size[[1]], 
      height = size[[2]])
  })
  
  
  nodeSizeNoOffset <- sapply(seq_along(parents), function(i){
    out <- grobNodeSize(node = i, parents, rawdata)
    # this spawns a new plot, and is probably the wrong approach
    # anyway
    c(width  = out[[1]], 
         height = out[[2]])
  })
  totals <- rowSums(nodeSizeNoOffset)
  widths <- nodeSizeNoOffset["width", ] / totals["width"] * 
              max(diff(range(x)), diff(range(y))) * widthMultiplier
  heights <- nodeSizeNoOffset["height", ] / totals["height"] * 
               max(diff(range(x)), diff(range(y))) * heightMultipler
  nodeSizeNoOffset <- lapply(seq_along(parents), function(i){
    c(width  = widths[i], 
      height = heights[i])
  })
  
  # plot the nodes
  # each node is plotted in its own viewport
  for (i in seq_along(parents)){
    grob <- grobNode(node = i, parents, rawdata)
    vpWidth <- unit(nodeSizeNoOffset[[i]][[1]], "native")
    vpHeight <- unit(nodeSizeNoOffset[[i]][[2]], "native")
    vp <- viewport(x      = unit(x[i], "native"),
                   y      = unit(y[i], "native"),
                   width  = vpWidth,
                   height = vpHeight,
                   clip   = "on")
    pushViewport(vp)
    grid.draw(grob)
    #grid.rect(gp = gpar(col = "red"))
    popViewport()
  }
  
  for (sj in seq_along(parents)){
    for (si in seq_along(parents[[sj]])){
      j <- sj # head
      i <- parents[[sj]][si] # tail
      
      if (!j %in% parents[[i]] | i < j){
        boundingRectPoints <- coordinatesBetweenRectangles(
                                                     x1 = x[i],
                                                     y1 = y[i],
                                                     x2 = x[j],
                                                     y2 = y[j],
                                                     w1 = nodeSize[[i]][1],
                                                     h1 = nodeSize[[i]][2],
                                                     w2 = nodeSize[[j]][1],
                                                     h2 = nodeSize[[j]][2])

        d1 <- distance(c(x[i], y[i]), c(boundingRectPoints[1],
                                        boundingRectPoints[2]))
        d2 <- distance(c(x[i], y[i]), c(boundingRectPoints[3],
                                        boundingRectPoints[4]))
        if (d2 < d1){
          a <- boundingRectPoints[1]
          b <- boundingRectPoints[2]
          boundingRectPoints[1] <- boundingRectPoints[3]
          boundingRectPoints[2] <- boundingRectPoints[4]
          boundingRectPoints[3] <- a
          boundingRectPoints[4] <- b
        }
        #panel.rect(xleft = x[j] - nodeSize[[j]][1]/2,
        #           ybottom = y[j] - nodeSize[[j]][2]/2,
        #           xright = x[j] + nodeSize[[j]][1]/2,
        #           ytop = y[j] + nodeSize[[j]][2]/2)
        
        if (!j %in% parents[[i]]){
          panel.arrows(x0     = boundingRectPoints[1],
                       y0     = boundingRectPoints[2],
                       x1     = boundingRectPoints[3],
                       y1     = boundingRectPoints[4],
                       length = unit(0.05, "native"),
                       col    = edgecol[i, j],
                       alpha  = edgealpha[i, j],
                       ...)
        }
        else {
          panel.arrows(x0     = boundingRectPoints[3],
                       y0     = boundingRectPoints[4],
                       x1     = boundingRectPoints[1],
                       y1     = boundingRectPoints[2],
                       length = unit(0.05, "native"),
                       col    = edgealpha[i, j],
                       alpha  = edgealpha[i, j],
                       ends   = "both",
                       ...)
        }
      }
    }
  }
}

#' Plot a graph
#' 
#' A generic
#' 
#' @param ... Passed to method
#' 
#' @export
grplot <- function(...){
  UseMethod("grplot")
}

#' Plot a parental graph
#' 
#' Plots a parental graph, by default using the layout routines of the 
#' package 'network'.
#' 
#' @param parents An object of class 'parental'
#' @param col A vector of colours for the nodes (FIXME - does not currently 
#'   work.)
#' @param alpha A vector of alpha values for the nodes (FIXME - does not 
#'   currently work.)
#' @param edgecol A matrix of edge colours.
#' @param edgealpha A matrix of edge alpha values
#' @param layout.par Passed to 
#'   \code{\link[network]{network.layout.fruchtermanreingold}}
#' @param grobNode A grob function that will be used to draw the nodes
#' @param grobNodeSize A function that can compute the sizes of the nodes
#' @param offset A offset
#' @param layout Optionally provide the coordinates at which each node will 
#'   be drawn. This should be supplied as a data.frame with columns 
#'   \code{xcoord} and \code{ycoord}. 
#' @param ... Further arguments (not currently passed on?)
#' @return A lattice plot
#' @S3method grplot parental
#' @export
grplot.parental <- function(parents,
                            col          = 1,
                            alpha        = 1,
                            edgecol, 
                            edgealpha    = 1,
                            layout.par   = list(niter = 100000), 
                            grobNode     = grobNodeName, 
                            grobNodeSize = grobNodeNameSize,
                            offset       = 0.25,
                            layout,
                            ...){
  stopifnot("parental" %in% class(parents))
  ocall <- sys.call(sys.parent())
  ccall <- match.call()
  
  ccall$panel <- panel.parental
  ccall$prepanel <- prepanel.parental
  if (is.null(names(parents))){
    names(parents) <- as.character(seq_along(parents))
  }
  ccall$parents <- parents
  
  adj <- as.adjacency(parents)
  hideIsolates <- F
  numberOfNodes <- length(parents)
  
  isolates <- which(rowSums(adj) == 0 & colSums(adj) == 0)
  if (hideIsolates){
    adj <- adj[-isolates, -isolates]
  }
  
  if (missing(layout)){
    net <- network(adj)
    layout <- network.layout.fruchtermanreingold(net, layout.par)
    layout <- as.data.frame(layout)
    colnames(layout) <- c("xcoord", "ycoord")
  }
  
  if (hideIsolates){
    newlayout <- matrix(NA, nrow = numberOfNodes, ncol = 2)
    temp <- setdiff(seq_len(numberOfNodes), isolates)
    newlayout[temp, ] <- as.matrix(layout)
    range <- range(layout$xcoord)
    s <- seq(from = range[1], to = range[2], length = length(isolates))
    x <- c(s, rep(min(layout$ycoord), length(isolates)))
    newlayout[isolates, ] <- matrix(x,
                                    byrow = T,
                                    nrow  = length(isolates),
                                    ncol  = 2)
    layout <- newlayout
  }
  
  inputs <- layout
  form <- ycoord ~ xcoord
  ccall$islist <- F
  
  ccall$col <- col
  ccall$alpha <- alpha
  if (!missing(edgecol)){
    ccall$edgecol <- edgecol
  }
  ccall$edgealpha <- edgealpha
  
  # axs means no extension of the xlim values from the prepanel function is
  # added. See Lattice (Sarkar) book page 141.
  ccall$scales <- list(draw = F,
                       axs = "i" # no padding on the axis
                       )
  ccall$aspect <- "fill"
  ccall$as.table <- T
  ccall$xlab <- list(NULL)
  ccall$ylab <- list(NULL)
  ccall$x <- form
  ccall$data <- inputs
  ccall$grobNodeSize <- grobNodeSize
  ccall$grobNode <- grobNode
  ccall$offset <- offset
  ccall[[1]] <- quote(lattice::xyplot)
  ans <- eval(ccall, parent.frame())
  ans$call <- ocall
  ans
}

#' Plot a parental.list
#' 
#' Plots a parental graph, by default using the layout routines of the 
#' package 'network'.
#'
#' @param parentallist An object of class 'parental.list'
#' @param col A vector of colours for the nodes (FIXME - does not currently 
#'   work.)
#' @param alpha A vector of alpha values for the nodes (FIXME - does not 
#'   currently work.)
#' @param edgecol A matrix of edge colours.
#' @param edgealpha A matrix of edge alpha values
#' @param layout.par Passed to 
#'   \code{\link[network]{network.layout.fruchtermanreingold}}
#' @param grobNode A grob function that will be used to draw the nodes
#' @param grobNodeSize A function that can compute the sizes of the nodes
#' @param offset A offset
#' @param ... Further arguments (not currently passed on?)
#' @return A lattice plot
#' @S3method grplot parental.list
#' @export
grplot.parental.list <- function(parentallist,
                                 col          = 1, 
                                 alpha        = 1,
                                 edgecol      = 1, 
                                 edgealpha    = 1,
                                 layout.par   = list(niter = 200000),
                                 grobNode     = grobNodeName, 
                                 grobNodeSize = grobNodeNameSize,
                                 offset       = 0.25,
                                 ...) 
{
  ocall <- sys.call(sys.parent())
  ocall[[1]] <- quote(hexbinplot)
  ccall <- match.call()
  
  ccall$panel <- panel.parental
  ccall$prepanel <- prepanel.parental
  
  for (i in seq_along(parentallist)){
    if (is.null(names(parentallist[[i]]))){
      names(parentallist[[i]]) <- as.character(seq_along(parentallist[[i]]))
    }
  }
  ccall$parents <- parentallist
  
  numberOfGraphs <- length(parentallist)
  numberOfNodes <- length(parentallist[[1]])
  
  adj <- as.adjacency(lpunion(parentallist))
  net <- network(adj)
  layout <- network.layout.fruchtermanreingold(net, layout.par)
  layout <- as.data.frame(layout)
  colnames(layout) <- c("xcoord", "ycoord")
  
  torbind <- lapply(seq_len(numberOfGraphs), function(i) layout)
  layouts <- do.call("rbind", torbind)
  graphIndicator <- gl(n      = numberOfGraphs,
                       k      = numberOfNodes,
                       length = numberOfGraphs * numberOfNodes)
  if (is.null(names(parentallist))){
    names(parentallist) <- seq_along(parentallist)
  }
  
  levels(graphIndicator) <- names(parentallist)
  inputs <- cbind(layouts, whichgraph = graphIndicator)
  form <- xcoord ~ ycoord | whichgraph
  ccall$islist <- T
  
  if (missing(edgecol)){
    edgecol <- matrix(1, ncol = numberOfNodes, nrow = numberOfNodes)
  }
  if (missing(edgealpha)){
    edgealpha <- matrix(1, ncol = numberOfNodes, nrow = numberOfNodes)
  }
  ccall$col <- col
  ccall$alpha <- alpha
  ccall$edgecol <- edgecol
  ccall$edgealpha <- edgealpha
  ccall$scales <- list(draw = F)
  ccall$as.table <- T
  ccall$xlab <- list(NULL)
  ccall$ylab <- list(NULL)
  ccall$grobNodeSize <- grobNodeSize
  ccall$grobNode <- grobNode
  ccall$offset <- offset
  ccall$x <- form
  ccall$data <- inputs
  ccall[[1]] <- quote(lattice::xyplot)
  ans <- eval(ccall, parent.frame())
  ans$call <- ocall
  ans
}


#' Plot a 'bvsresponse' graph. 
#' 
#' @param x A 'bvsresponse' object
#' @param col A vector with each component indicating the colour of the 
#'   corresponding node. The default character vector vector "default"
#'   makes the response node red and the other nodes black.
#' @param ... further arguments
#' @return A lattice plot of the graph
#' @S3method grplot bvsresponse
#' @export
grplot.bvsresponse <- function(x, col = "default", ...){
  stopifnot(class(x) == "bvsresponse")
  bvs <- as.bvs(x)
  response <- x$response
  nNodes <- x$nNodes
  
  if (col == "default"){
    col <- rep(1, nNodes) # 1 = black
    col[response] <- 2 # 2 = red
  }
  grplot(bvs, col = col, ...)
}

#' Multi-page plot of nodeLevelPlots
#' 
#' Creates a plot with a level plot for each variable, each of which is 
#' presented on a separate page.
#' 
#' @param parents An object of class 'parental'
#' @param rawdata A data frame.
#' @export
nodeLevelplot <- function(parents, rawdata){
  for (node in seq_along(parents)){
    grid.newpage()
    grob <- grobNodeLevelPlot(node, parents, rawdata, strip.lines = 2,
                              strip.left.lines = 2, 
                              theme = standard.theme())
    grid.draw(grob)
  }
}