
riskScatterRawFn = function(pos, r1, r2, col = "#0000FF40", pch = 19, cex = 0.6) {
  if (!"package:grid" %in% search()) {
    print("Loading 'grid' package")
    library(grid)
    print("Done loading 'grid' package")
  }
  
  if( ! grepl("layoutVp", as.character(current.vpTree()))) {
    stop("setUpTrellisFn() must be called before this function.")
  }

  if( length(r1) != length(r2) )
    stop("r1 and r2 need to be vectors of the same length")
  
  downViewport(paste(pos[1], pos[2]))
  on.exit(upViewport(0))

  grid.points(x = r1, y = r2, pch = pch, gp = gpar(col = col, cex = cex))

  invisible( list( r1, r2) )

}
