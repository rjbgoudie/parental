#' Put Strips on the Boundary of a Lattice Display.
#' 
#' Try to update a "trellis" object so that strips are only shown on the 
#' top and left boundaries when printed, instead of in every panel as is 
#' usual. This version extend the version in LatticeExtra to allow more than 
#' two conditioning variables.
#' 
#' @param x An object of class "trellis".
#' @param strip A function, character string, or logical that would 
#'   be appropriate strip and strip.left arguments respectively in a high 
#'   level lattice function call (see xyplot). Note, however, that the strip 
#'   function must be of the form of strip.default2. 
#'   The equivalent of strip.custom here is strip.custom2.
#' @param strip.left As strip, for the strips down the left.
#' @param top Determines the number of strips that are drawn along the top 
#'   of the plot. Conditioning variables 1, ..., top will be displayed along 
#'   the top of the plot (using strip), and variables top + 1, ..., dim(x) 
#'   will be displayed along the left hand side of the plot.
#' @param strip.lines height of strips in number of lines; helpful for 
#'   multi-line text or mathematical annotation in strips.
#' @param strip.left.lines As strip.lines, for strips down the left.
#' @param horizontal ...
#' @return An object of class "trellis", essentially the same as x, but with 
#'   certain properties modified.
#' @export
useOuterStrips2 <- 
    function(x,
             strip = strip.default2,
             strip.left = strip.custom2(horizontal = horizontal),
             top = 1, 
             strip.lines = 1,
             strip.left.lines = strip.lines,
             horizontal = F){
  dimx <- dim(x)
  stopifnot(inherits(x, "trellis"))
  
  topSeq <- seq_len(top)
  topdimx <- dimx[topSeq]
  leftdimx <- dimx[-topSeq]
  
  opar <- if (is.null(x$par.settings)) list() else x$par.settings
  par.settings <-
      modifyList(opar,
                 list(layout.heights =
                      if (x$as.table){
                        list(strip = c(strip.lines * top, rep(0, 
                                        prod(leftdimx)-1)))
                      }
                      else {
                        list(strip = c(rep(0, prod(leftdimx)-1), 
                                strip.lines * top))
                      },
                      layout.widths =
                      list(strip.left = c(strip.left.lines * length(leftdimx),
                                        rep(0, prod(topdimx)-1)))))
  if (is.character(strip)){
      strip <- get(strip)
  }
  if (is.logical(strip) && strip){
      strip <- strip.default2
  }
  new.strip <-
      if (is.function(strip))
      {
          function(which.given, which.panel, var.name, ...) {
            given.top <- which.given <= top
            if (given.top){
              topdimx <- topdimx
              leftdimx <- leftdimx
              prev.dim <- topdimx[seq_len(which.given - 1)]
              
              is.top.row <- current.row() == prod(leftdimx)
              is.level.change <- current.column() %% prod(prev.dim) == 1
              is.first.dim <- which.given == 1
              if (is.top.row && (is.level.change || is.first.dim)){
                    strip(which.given = which.given,
                          which.panel = which.panel[topSeq],
                          var.name    = var.name[topSeq],
                          dimx        = topdimx,
                          ...)
              }
            }
          }
      }
      else {
        strip
      }
  if (is.character(strip.left)){
      strip.left <- get(strip.left)
  }
  if (is.logical(strip.left) && strip.left){
      strip.left <- strip.custom2(horizontal = FALSE)
  }
  
  new.strip.left <-
      if (is.function(strip.left)){
          function(which.given, which.panel, var.name, ...) {
            which.given <- which.given - top
            given.left <- which.given >= 1
            if (given.left){
              leftdimx <- leftdimx
              prev.dim <- leftdimx[seq_len(which.given - 1)]
              
              is.left.col <- current.column() == 1
              is.level.change <- current.row() %% prod(prev.dim) == 1
              is.first.dim <- which.given == 1
              
              if (is.left.col && (is.level.change || is.first.dim)){
                  strip.left(which.given  = which.given,
                              which.panel = which.panel[-topSeq],
                              dimx        = leftdimx,
                              var.name    = var.name[-topSeq],
                              ...)
              }
            }
          }
      }
      else {
        strip.left
      }
  update(x,
         par.settings   = par.settings,
         strip          = new.strip,
         strip.left     = new.strip.left,
         par.strip.text = list(lines = 0.5),
         layout         = c(prod(topdimx), prod(leftdimx)))
}

#' Undocumented.
#' 
#' ...
#' 
#' @param ... ...
strip.custom2 <-
    function(...)
{
    args <- list(...)
    function(...)
    {
        dots <- list(...)
        do.call("strip.default2",
                lattice:::updateList(dots, args))
    }
}

#' Undocumented.
#' 
#' description
#' 
#' @param which.given ...
#' @param which.panel ...
#' @param var.name ...
#' @param factor.levels ...
#' @param shingle.intervals ...
#' @param strip.names ...
#' @param strip.levels ...
#' @param sep ...
#' @param style ...
#' @param horizontal ...
#' @param bg ...
#' @param fg ...
#' @param par.strip.text ...
#' @param dimx ADDED ...
strip.default2 <-
    function(which.given,
             which.panel,
##              packet.number,
##              panel.number,
             var.name,
             factor.levels,
             shingle.intervals = NULL,
             strip.names = c(FALSE, TRUE),
             strip.levels = c(TRUE, FALSE),
             sep = " : ",
             style = 1,
             horizontal = TRUE,
             ## FIXME: not sure how to incorporate alpha in strip colors
             bg = trellis.par.get("strip.background")$col[which.given],
             fg = trellis.par.get("strip.shingle")$col[which.given],
             par.strip.text = trellis.par.get("add.text"),
             dimx)
{
    prev.dim <- dimx[seq_len(which.given - 1)]
    extent <- prod(prev.dim)

    if (horizontal)
        lattice:::pushViewport(lattice:::viewport(y = 
                                (which.given-0.5)/length(which.panel),
                              height = 1/length(which.panel),
                              width = extent,
                              x = extent/2,
                              clip = trellis.par.get("clip")$strip,
                              name = paste("strip.default", which.given,
                                           sep = ".")))
    else 
        lattice:::pushViewport(lattice:::viewport(x = 
                                1 - (which.given-0.5)/length(which.panel),
                              width = 1/length(which.panel),
                              height = extent,
                              y = extent/2,
                              clip = trellis.par.get("clip")$strip,
                              name = paste("strip.default", which.given,
                                           sep = ".")))


    gp.text <- 
        lattice:::gpar(col = par.strip.text$col,
             alpha = par.strip.text$alpha,
             lineheight = par.strip.text$lineheight,
             fontfamily = par.strip.text$fontfamily,
             fontface = lattice:::chooseFace(par.strip.text$fontface,
                                             par.strip.text$font),
             cex = par.strip.text$cex)

    name <- var.name[which.given]
    level <- which.panel[which.given]
    strip.names <- rep(strip.names, length.out = 2)
    strip.levels <- rep(strip.levels, length.out = 2)
    ## str(shingle.intervals)

    formatLabel <-
        function(s,
                 abbreviate = par.strip.text$abbr,
                 minlength = par.strip.text$minl,
                 dot = par.strip.text$dot)
    {
        if (is.null(abbreviate)) abbreviate <- FALSE
        if (is.null(minlength)) minlength <- 4
        if (is.null(dot)) dot <- FALSE
        if (abbreviate) abbreviate(s, minlength = minlength, dot = dot)
        else s
    }
    factor.levels <- formatLabel(factor.levels)
    
    if (!is.null(shingle.intervals))
    {

        ## This usually indicates shingles, as opposed to factors.
        ## 'style' will be completely ignored, and shingle.intervals
        ## encoded using bg and fg.  Names and levels are both game.

        lattice:::grid.rect(gp = lattice:::gpar(fill = bg, col = bg))

        t <- range(shingle.intervals)
        r <- (range(shingle.intervals[level,]) - t[1]) / diff(t)
        if (horizontal)
            lattice:::grid.rect(x = lattice:::unit(r %*% c(.5,.5),"npc"),
                      width = max(lattice:::unit(c(diff(r), 1), 
                                                        c("npc", "mm"))),
                      gp = lattice:::gpar(col = fg, fill = fg))
        else 
            lattice:::grid.rect(y = lattice:::unit(r %*% c(.5,.5),"npc"),
                      height = max(lattice:::unit( c(diff(r), 1), 
                                                        c("npc", "mm"))),
                      gp = lattice:::gpar(col = fg, fill = fg))

        lattice:::paste.and.draw(name, factor.levels[level],
                       sep = sep,
                       horizontal = horizontal,
                       showl = strip.names[2],
                       showr = strip.levels[2],
                       gp = gp.text)
    }
    else
    {
        ## Behaviour depends on 'style'.  Will separate out coloring
        ## and text based on 'style'.
        
        num <- length(factor.levels)

        ## coloring:

        ## background: all except style = 2
        if (style != 2) lattice:::grid.rect(gp = lattice:::gpar(fill = bg,
                                                                col = bg))

        ## foreground: needed only for style = 2, 3 and 4

        if (num > 0 && style %in% c(2, 3, 4))
        {
            if (horizontal)
            {
                lattice:::grid.rect(x = 
                                  lattice:::unit((2*level-1)/(2*num), "npc"),
                          width = lattice:::unit(1/num, "npc"),
                          gp = lattice:::gpar(fill = fg, col = fg))
            }
            else
            {
                lattice:::grid.rect(y = 
                                  lattice:::unit((2*level-1)/(2*num), "npc"),
                          height = lattice:::unit(1/num, "npc"),
                          gp = lattice:::gpar(fill = fg, col = fg))
            }
        }

        ## text: [names|levels] centered only if style = 1 or 3

        if (style %in% c(1, 3))
        {
            paste.and.draw2(name, factor.levels[level],
                           sep = sep,
                           horizontal = T,
                           showl = strip.names[1],
                           showr = strip.levels[1],
                           gp = gp.text)
        }
        ## remaining cases
        else if (num > 0)
        {
            ## either all levels or only one
            lid <- if (style %in% c(2, 4)) 1:num else level
            if (horizontal)
            {
                lattice:::grid.text(label = factor.levels[lid],
                          x = (2 * lid - 1) / (2 * num),
                          gp = gp.text)
            }
            else
            {
                lattice:::grid.text(label = factor.levels[lid],
                          y = (2 * lid - 1) / (2 * num),
                          gp = gp.text)
            }
        }
    }
    lattice:::upViewport()

    ## border is drawn with clipping off
    if (horizontal)
        lattice:::pushViewport(lattice:::viewport(y = 
                                  (which.given-0.5)/length(which.panel),
                              height = 1/length(which.panel),
                              width = extent,
                              x = extent/2,
                              clip = "off",
                              name = paste("strip.default.off", 
                                           which.given, sep = ".")))
    else 
        lattice:::pushViewport(lattice:::viewport(x = 
                                  1 - (which.given-0.5)/length(which.panel),
                              width = 1/length(which.panel),
                              height = extent,
                              y = extent/2,
                              clip = "off",
                              name = paste("strip.default.off",
                                           which.given, sep = ".")))


    strip.border <- trellis.par.get("strip.border")
    ## draw border for strip
    lattice:::grid.rect(gp =
              lattice:::gpar(col = rep(strip.border$col,
                             length.out = which.given)[which.given],
                   lty = rep(strip.border$lty,
                             length.out = which.given)[which.given],
                   lwd = rep(strip.border$lwd,
                             length.out = which.given)[which.given],
                   alpha = rep(strip.border$alpha,
                               length.out = which.given)[which.given],
                   fill = "transparent"))
    lattice:::upViewport()
}

#' Undocumented.
#' 
#' description
#' 
#' @param left ...
#' @param right ...
#' @param sep ...
#' @param horizontal ...
#' @param center ...
#' @param showl ...
#' @param showr ...
#' @param gp ...
paste.and.draw2 <- function(left, right, sep = " : ", horizontal = TRUE, 
    center = TRUE, showl = TRUE, showr = TRUE, gp = lattice:::gpar()) 
{
    if (showl || showr) {
        shows <- showl && showr
        wsep <- lattice:::unit(0.5 * shows, "strwidth", list(sep))
        offset <- lattice:::unit(0.5, "npc")
        if (center) 
            offset <- offset + (if (showl) 
                lattice:::unit(0.5, "strwidth", list(left))
            else lattice:::unit(0, "mm")) - (if (showr) 
                lattice:::unit(0.5 * showr, "strwidth", list(right))
            else lattice:::unit(0, "mm"))
        if (horizontal) {
            if (shows) 
                lattice:::grid.text(sep, x = offset, gp = gp)
            if (showl) 
                lattice:::grid.text(left, x = offset - wsep,
                                    gp = gp, just = "right")
            if (showr) 
                lattice:::grid.text(right, x = offset + wsep, gp = gp, 
                  just = "left")
        }
        else {
            if (shows) 
                lattice:::grid.text(sep, y = offset, gp = gp, rot = 0)
            if (showl) 
                lattice:::grid.text(left, y = offset - wsep, gp = gp,
                                    just = "right", rot = 0)
            if (showr) 
                lattice:::grid.text(right, y = offset + wsep, gp = gp, 
                  just = "left", rot = 0)
        }
    }
}

