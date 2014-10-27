  riskCdfRawFn = function(pos, x1, a1, col = "#B23AEE", pch = 20, cex = 1){
  
    if (!"package:grid" %in% search()) {
      print("Loading 'grid' package")
      library(grid)
      print("Done loading 'grid' package")
    }
    
    if( ! grepl("layoutVp", as.character(current.vpTree()))) {
      stop("setUpTrellisFn() must be called before this function.")
    }
    
    if(!(is.numeric(x1) && is.numeric(a1) && (length(x1) == length(a1)))) {
      stop("x1 and a1 must be numeric vectors with the same length.")
    }
    
    downViewport(paste(pos[1], pos[2]))
    on.exit(upViewport(0))
    
    F = ecdf2Stg(x1, a1)
    x = sort(unique(x1))
    y = F(x)
    x0 = c(0,x)
    y0 = c(0,y)
    x1 = c(x, 1)
    y1 = y0
    grid.points(x0, y0, pch = pch, gp = gpar(col = col), 
      size = unit(cex, "char"))
    grid.segments(x0 = x0, y0 = y0, x1 = x1, y1 = y1,
      default.units = "native",
      gp = gpar(col = col))
    invisible(list(x1 = x1, a1 = a1))
  }
  
# Wed Sep 21 20:56:05 PDT 2011
