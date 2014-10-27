
riskDensityRawFn = function(pos, e, t, r, tStar, col1, col2, lightCol1, lightCol2, adjust = 1,
  legend = TRUE) {

  if (!"package:grid" %in% search()) {
    print("Loading 'grid' package")
    library(grid)
    print("Done loading 'grid' package")
  }
  
  if( ! grepl("layoutVp", as.character(current.vpTree()))) {
    stop("setUpTrellisFn() must be called before this function.")
  }

  downViewport(paste(pos[1], pos[2]))
  on.exit(upViewport(0))

  affected = r[e == 1]
  unaffected = r[ ((e == 0) & (t > (tStar - 0.01))) | (e == 2) ]

  dens_unaffected = density(unaffected, from = 0, to = 1, adjust = adjust)
  dens_affected = density(affected, from = 0, to = 1, adjust = adjust)

  grid.polygon(x = c(dens_unaffected$x[1], dens_unaffected$x, dens_unaffected$x[length(dens_unaffected$x)]),
               y = c(0, dens_unaffected$y, 0),
               gp = gpar(col = col1, fill = lightCol1), default.units = "native")

  grid.polygon(x = c(dens_affected$x[1], dens_affected$x, dens_affected$x[length(dens_affected$x)]),
               y = c(0, dens_affected$y, 0),
               gp = gpar(col = col2, fill = lightCol2), default.units = "native")


  if(legend) {
    
    legendvp = viewport(x = unit(1, "npc") - unit(1, "lines"),
                        y = unit(1, "npc") - unit(1, "lines"),
                        just = c("right", "top"),
                        height = unit(3, "lines"),
                        width = stringWidth("unaffected") + unit(3, "lines"), name = "legendvp")
    pushViewport(legendvp)
    grid.rect()

    grid.text(label = c("unaffected", "affected"),
              x = unit(2, "lines"), y = unit(1:2, "lines"), just = "left")

    grid.rect(x = unit(1, "lines"), y = unit(1:2, "lines"),
              height = unit(0.8, "lines"), width = unit(0.8, "lines"),
              gp = gpar(col = c(col1, col2), fill = c(lightCol1, lightCol2)))
    
  }

  invisible(
            list( pos = pos, e = e, t = t, r = r, tStar = tStar, col1 = col1, col2 = col2, lightCol1 = lightCol1,
                 lightCol2 = lightCol2, adjust = adjust)
          )
}
