## Tue Aug 30 15:40:53 PDT 2011
## est has been changed from a data.frame to a matrix and so we needed to change est$rho to est[, "rho"].


IAD_RawFn = function(pos, rvu, color, lightCol) {

  if (!"package:grid" %in% search()) {
    print("Loading 'grid' package")
    library(grid)
    print("Done loading 'grid' package")
  }

  if( ! grepl("layoutVp", as.character(current.vpTree()))) {
    stop("setUpTrellisFn() must be called before this function.")
  }

  if(! "rvu" %in% class(rvu))
    stop("The 'rvu' argument must have class 'rvu'.")
  
  downViewport(paste(pos[1], pos[2]))
  on.exit(upViewport(0))

  if("confBand" %in% names(rvu$PNN)) {
    grid.polygon(x = c(rvu$PNN$est[, "rho"], rev(rvu$PNN$est[, "rho"])),
                 y = c(rvu$PNN$confBand[,1], rev(rvu$PNN$confBand[,2])),
                 default.units = "native",
                 gp = gpar(fill = lightCol, col = color))
  }
  
  grid.lines(x = rvu$PNN$est[, "rho"],
             y = rvu$PNN$est[,"piHatNN"],
             default.units = "native",
             gp = gpar(lwd = 2, col = color))

  grid.lines(x = c(0, 1), y = c(0, 1), 
             default.units = "npc", gp = gpar(lty = 2))

#   grid.rect()
  
  invisible(rvu)
}

