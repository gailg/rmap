

setUpTrellisFn = function(nrow, ncol, main = NULL,
  xlab = if(axesInPercents) "assigned risk (%)" else "assigned risk",
  ylab = if(axesInPercents) "observed risk (%)" else "observed risk",
  xmax = 100, ymax = 100, axesInPercents = TRUE, inflate = 1.1, newpage = TRUE, atX = NULL, atY = NULL)
{
  
  if (!"package:grid" %in% search()) {
    print("Loading 'grid' package")
    library(grid)
    print("Done loading 'grid' package")
  }

  if(axesInPercents) {
    xmax = xmax / 100
    ymax = ymax / 100
  }

  xmaxActual = xmax * inflate
  ymaxActual = ymax * inflate
  
  if(newpage) grid.newpage()

  if(is.null(main)) {
    pushViewport(plotViewport(c(5, 4, 2, 2), name = "plotVp"))
  } else {
    pushViewport(plotViewport(c(5, 4, 3, 2), name = "plotVp"))
    grid.text(label = main,
              x = unit(0.5, "npc"),
              y = unit(1, "npc") + unit(1, "lines"),
              gp = gpar(cex = 1.3))
  }
  on.exit(upViewport(0))
  
  grid.rect(gp = gpar(fill = gray(0.95)))

  if(!is.null(xlab)) grid.text(label = xlab,
            x = unit(0.5, "npc"), y = unit(-3, "lines"))

  if(!is.null(ylab)) grid.text(label = ylab,
            x = unit(-3, "lines"), y = unit(0.5, "npc"), rot = 90)

  layoutVp = viewport(layout = grid.layout(nrow = nrow, ncol = ncol),
    name = "layoutVp")
  pushViewport(layoutVp)
  
  for(j in seq_len(ncol)) for(i in seq_len(nrow)) {
    pushViewport(viewport(layout.pos.row = i, layout.pos.col = j,
                          xscale = c(0, xmaxActual * if(axesInPercents) 100 else 1),   # times 100 for percents
                          yscale = c(0, ymaxActual * if(axesInPercents) 100 else 1)))  # times 100 for percents

    #browser()

    if(is.null(atX)) {
      firstPart = seq(0, xmax, length = if(ncol >= 3) 3 else 5)[2]
      rounded = round(firstPart * (if(axesInPercents) 100 else 1), if(axesInPercents) 0 else 2)
      atX = rounded * (0:(if(ncol >= 3) 3 else 5))
    }
    if(is.null(atY)) {
      firstPart = seq(0, ymax, length = if(ncol >= 3) 3 else 5)[2]
      rounded = round(firstPart * (if(axesInPercents) 100 else 1), if(axesInPercents) 0 else 2)
      atY = rounded * (0:(if(ncol >= 3) 3 else 5))
    }

    atX = atX[atX < (xmaxActual * if(axesInPercents) 100 else 1)]
    atY = atY[atY < (ymaxActual * if(axesInPercents) 100 else 1)]
      
    if(i == nrow) grid.xaxis(at = atX)
    if(j == 1) grid.yaxis(at = atY)

    xmaxActualPerc = xmaxActual * if(axesInPercents) 100 else 1
    ymaxActualPerc = ymaxActual * if(axesInPercents) 100 else 1

    grid.segments( x0 = atX[! atX %in% c(0, xmaxActualPerc)], y0 = 0,
                   x1 = atX[! atX %in% c(0, xmaxActualPerc)], y1 = ymaxActualPerc ,
                  gp = gpar(col = "white", lwd = 1, lineend = "butt"), default.units = "native")
    grid.segments( x0 = 0, y0 = atY[! atY %in% c(0, ymaxActualPerc)],
                  x1 = xmaxActualPerc, y1 = atY[! atY %in% c(0, ymaxActualPerc)],
                  gp = gpar(col = "white", lwd = 1, lineend = "butt"), default.units = "native")
    popViewport()
    
    pushViewport(viewport(layout.pos.row = i, layout.pos.col = j,
                          xscale = c(0, xmaxActual),
                          yscale = c(0, ymaxActual), name = paste(i, j), clip = "on"))
    grid.rect(gp = gpar(lwd = 1))
    upViewport()
  }

  list(nrow = nrow, ncol = ncol, main = main, ylab = ylab,
       xlab = xlab, xmax = xmax, ymax = ymax, inflate = inflate,
       xmaxActual = xmaxActual, ymaxActual = ymaxActual)
}
       
