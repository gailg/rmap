

## Fri Aug 12 11:28:37 PDT 2011
## Copied from inside
##    trunk/projects/riskModel/53-nearest-neighbor/03-scribbles/18o-modified-plots-as-per-Alices-instructions-r2.R

attributeDiagramRawFn = function(pos, rv, color, lightCol = "#FFFFFF00") {
  
  if (!"package:grid" %in% search()) {
    print("Loading 'grid' package")
    library(grid)
    print("Done loading 'grid' package")
  }

  if( ! grepl("layoutVp", as.character(current.vpTree()))) {
    stop("setUpTrellisFn() must be called before this function.")
  }

  if(! "rv" %in% class(rv))
    stop("The 'rv' argument must have class 'rv'.")

  downViewport(paste(pos[1], pos[2]))
  on.exit(upViewport(0))
  
  width = convertX(unit(1 / 50, "npc"), "native")
 
    if ("lowerBoot" %in% names(rv$piHatSummary)) {
      apply(rv$piHatSummary[, c("r", "lowerBoot", "upperBoot")],
            1, function(bootRow)
      {
        grid.rect(x = bootRow[1], y = mean(bootRow[2:3]), 
                  width = width, height = bootRow[3] - bootRow[2], 
                  just = c("center", "center"), default.units = "native", 
                  gp = gpar(fill = lightCol, col = lightCol))
      })
    }
  
    apply(rv$piHatSummary[, c("r", "lower", "upper", "piHat")],
          1, function(row)
    {
      grid.lines(x = c(row[1], row[1]), y = c(row[2], row[3]), 
                 default.units = "native", gp = gpar(col = color, lwd = 1,
                                             lineend = "square"))
      grid.lines(x = unit.c(unit(row[1], "native") - width, unit(row[1], "native") + width), 
                 y = c(row[2], row[2]), default.units = "native", 
                 gp = gpar(col = color, lwd = 1, lineend = "square"))
      grid.lines(x = unit.c(unit(row[1], "native") - width, unit(row[1], "native") + width), 
                 y = c(row[3], row[3]), default.units = "native", 
                 gp = gpar(col = color, lwd = 1, lineend = "square"))
      
      grid.points(row[1], row[4], pch = 19, gp = gpar(col = color))
      grid.points(row[1], row[4], pch = 19, gp = gpar(col = "white", cex = 0.5))
    })

  grid.lines(x = c(0, 1), y = c(0, 1), 
             default.units = "npc", gp = gpar(lty = 2))
  
  invisible(rv)
}

