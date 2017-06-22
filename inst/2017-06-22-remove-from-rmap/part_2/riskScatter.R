

riskScatter = function(r1, r2, 
  rvpar = rvparFn(col = "#0000FF40"), ocall = NULL) {

  if (!"package:grid" %in% search()) {
    print("Loading 'grid' package")
    library(grid)
    print("Done loading 'grid' package")
  }
  
  if (is.null(ocall)) 
    ocall = deparse(match.call())

  if(is.null(rvpar$xlab)) rvpar$xlab = if(rvpar$axesInPercents) "risk (%)" else "risk"
  if(is.null(rvpar$ylab)) rvpar$ylab = if(rvpar$axesInPercents) "risk (%)" else "risk"
    
  grid.newpage()
  vpbottom = viewport(x = unit(0.5, "cm"), y = unit(0.25, "cm"),
    width = unit(1, "npc") - unit(1, "cm"),
    height = unit(5, "lines") - unit(0.5, "cm"),
    just = c("left", "bottom"), name = "vpbottom", clip = "on")
  vptop = viewport(x = 0, y = unit(5, "lines"),
    width = unit(1, "npc"), height = unit(1, "npc") - unit(5, "lines"),
    just = c("left", "bottom"), name = "vptop")

  ##----------------------------bottom viewport
  pushViewport(vpbottom)
  on.exit(upViewport(0))
  
  grid.rect()
  currentDate = as.character(Sys.time())
  currentCall = ocall
  grid.text(paste(rvpar$comment, currentDate, currentCall, sep = "\n"),
            x = unit(1.5, "cm"), y = unit(0.5, "npc"), 
            just = c("left", "center"))

  upViewport()

 
  #if (is.null(rvpar$xymax))
  xymax = (max(c(r1, r2)) + 0.15) * if(rvpar$axesInPercents) 100 else 1
  if (is.null(rvpar$xmax)) rvpar$xmax = xymax
  if (is.null(rvpar$ymax)) rvpar$ymax = xymax
  
  pushViewport(vptop)

  #browser()
  
  setUpTrellisFn(nrow = 1, ncol = 1, main = rvpar$main,
                 xlab = rvpar$xlab, ylab = rvpar$ylab, atX = rvpar$atX, atY = rvpar$atY,
                 axesInPercents = rvpar$axesInPercents,
                 xmax = rvpar$xmax, ymax = rvpar$ymax, inflate = rvpar$inflate,
                 newpage = FALSE)

  riskScatterRawFn(pos = c(1, 1), r1 = r1, r2 = r2, col = rvpar$col[1],
                   pch = rvpar$pch, cex = rvpar$cex)

  invisible( list( r1, r2) ) 
  
}
