
# This will be a "level-3" function.
# 1) Creates a lower box that holds information to help remember
#    when/how the graph was created, such as:
#      - ocall
#      - date
#      - annotation
# 2) calls setUpTrellisFn() for 1x1 in the upper region
# 3) calls attributeDiagramRawFn()

attributeDiagram = function(rvs,
  rvpar = rvparFn(xlab = "assigned risk (%)", ylab = "observed risk (%)"), ocall = NULL) {

  if (!"package:grid" %in% search()) {
    print("Loading 'grid' package")
    library(grid)
    print("Done loading 'grid' package")
  }

  if (is.null(ocall)) 
    ocall = deparse(match.call())

  if ("rv" %in% class(rvs)) {
    rvs = list(rvs)
  } else if (!all(sapply(rvs, function(rv) "rv" %in% class(rv)))) {
    stop("Make sure all list elements have class 'rv'.")
  }
  if (length(rvs) > length(rvpar$col)) 
    stop("Please supply enough colors in rvpar.")

  if( (length(rvs) > 1) && rvpar$annotate)
    rvpar$annotate = FALSE

  if(is.null(rvpar$xlab)) rvpar$xlab = if(rvpar$axesInPercents) "assigned risk (%)" else "assigned risk"
  if(is.null(rvpar$ylab)) rvpar$ylab = if(rvpar$axesInPercents) "observed risk (%)" else "observed risk"  
  
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

  ##----------------------------xymax
  ## if (is.null(rvpar$xymax)) {
    xmaxs = sapply(rvs, function(rv) max(rv$piHatSummary$r))
    ymaxs = sapply(rvs, function(rv) max(rv$piHatSummary$upper))
    xymax = max(c(xmaxs, ymaxs)) + 0.15
  ## }
  if (is.null(rvpar$xmax)) rvpar$xmax = xymax * if(rvpar$axesInPercents) 100 else 1
  if (is.null(rvpar$ymax)) rvpar$ymax = xymax * if(rvpar$axesInPercents) 100 else 1

  ##----------------------------top viewport  
  pushViewport(vptop)
  setUpTrellisFn(nrow = 1, ncol = 1, main = rvpar$main,
                 xlab = rvpar$xlab, ylab = rvpar$ylab,
                 xmax = rvpar$xmax, ymax = rvpar$ymax, axesInPercents = rvpar$axesInPercents,
                 inflate = rvpar$inflate,
                 newpage = FALSE, atX = rvpar$atX, atY = rvpar$atY)

  sapply(seq_along(rvs), function(i) {
    attributeDiagramRawFn(pos = c(1, 1), rv = rvs[[i]], col = rvpar$col[i], lightCol = rvpar$lightCol[i] )
  })

  ##----------------------------annotate 
  if(rvpar$annotate) {
    
    prettyText = paste0("HL ChiSq: ", round(rvs[[1]]$ChiSq["HosmerLemeshow"], 
      4), ", df: ", nrow(rvs[[1]]$piHatSummary), ", p-val: ",
      round(rvs[[1]]$ChiSq["HL_pval"], 4))

  addTextToTrellisFn(pos = c(1, 1), text = prettyText, where = "topleft")

  }

  invisible(rvs)
  
}
