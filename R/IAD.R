## Tue Aug 30 15:40:53 PDT 2011
## est has been changed from a data.frame to a matrix and so we needed to change est$rho to est[, "rho"].

IAD = function( rvus, 
  rvpar = rvparFn(lightCol = paste(rvparFn()$col, "30", sep = "")), ocall = NULL) {

  if (!"package:grid" %in% search()) {
    print("Loading 'grid' package")
    library(grid)
    print("Done loading 'grid' package")
  }
  
  if (is.null(ocall)) 
    ocall = deparse(match.call())

  if ("rvu" %in% class(rvus)) {
    rvus = list(rvus)
  } else if (!all(sapply(rvus, function(rvu) "rvu" %in% class(rvu)))) {
    stop("Make sure all list elements have class 'rv'.")
  }
  
  if (length(rvus) > length(rvpar$col)) 
    stop("Please supply enough values for 'col' in rvpar.")

  if(length(rvus) > length(rvpar$lightCol))
    stop("Please supply enough values for 'lightCol' in rvpar.")
  
  if( (length(rvus) > 1) && rvpar$annotate)
    rvpar$annotate = FALSE

  if(length(rvus) >= 2) {
    # convert non-hex lightCol to hex format:
    whichSpecial = which(substr(rvpar$lightCol, 1, 1) != "#")
    rvpar$lightCol[whichSpecial] = apply(col2rgb(rvpar$lightCol[whichSpecial]), 2, function(col)
                  rgb(col[1], col[2], col[3], 48, maxColorValue = 255))

    # change the hex colors that AREN'T already transparent TO a transparent version of that color
    rvpar$lightCol[nchar(rvpar$lightCol) == 7] = paste(rvpar$lightCol[nchar(rvpar$lightCol) == 7],
                    "50", sep = "")
  }

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
  ## if(is.null(rvpar$xymax)) {
    xymaxs = sapply(rvus, function(rvu) {
      estMax = max(rvu$PNN$est[, "rho"])
      if("confBand" %in% names(rvu$PNN)) {
        confBandMax = max(rvu$PNN$confBand)
        max(confBandMax, estMax)
      } else {
        estMax
      }
    })
    xymax = max(xymaxs)
  
  if (is.null(rvpar$xmax)) rvpar$xmax = xymax * if(rvpar$axesInPercents) 100 else 1
  if (is.null(rvpar$ymax)) rvpar$ymax = xymax * if(rvpar$axesInPercents) 100 else 1
  ## }
  
  ##----------------------------top viewport
  pushViewport(vptop)

#  browser()
  
  setUpTrellisFn(nrow = 1, ncol = 1, main = rvpar$main,
                 xlab = rvpar$xlab, ylab = rvpar$ylab, axesInPercents = rvpar$axesInPercents,
                 xmax = rvpar$xmax, ymax = rvpar$ymax, inflate = rvpar$inflate,
                 newpage = FALSE, atX = rvpar$atX, atY = rvpar$atY)

  sapply(seq_along(rvus), function(i) {
    IAD_RawFn(pos = c(1, 1), rvu = rvus[[i]], col = rvpar$col[i], lightCol = rvpar$lightCol[i] )
  })

#   ##----------------------------annotate
#   if(rvpar$annotate) {
#     prettyText = paste("AUC: ", round(rvus[[1]]$AUC$AUC, 4),
#       ", CI: (", round(rvus[[1]]$AUC$AUC_CI[1], 4), ", ",
#       round(rvus[[1]]$AUC$AUC_CI[2], 4), ")", sep = "")
# 
#     addTextToTrellisFn(pos = c(1, 1), text = prettyText, where = "topleft")
#   }
  invisible(rvus)
}
