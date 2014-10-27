  riskCdf = function(crps, 
    rvpar = rvparFn(xlab = "CRP (%)", ylab = "F(CRP) (%)"), ocall =  NULL) {
    
    if (!"package:grid" %in% search()) {
      print("Loading 'grid' package")
      library(grid)
      print("Done loading 'grid' package")
    }
  
    if (is.null(ocall)) 
      ocall = deparse(match.call())
   
    if ("crp" %in% class(crps)){
      crps = list(crps)
    } else if (!all(sapply(crps, function(crp) "crp" %in% class(crp)))) {
      stop("Make sure all list elements have class 'crp'.")
    }
  
    if (length(crps) > length(rvpar$col)) 
      stop("Please supply enough colors in rvpar.")
  
    if(is.null(rvpar$xlab)) rvpar$xlab = if(rvpar$axesInPercents) "CRP (%)" else "CRP"
    if(is.null(rvpar$ylab)) rvpar$ylab = if(rvpar$axesInPercents) "F(CRP) (%)" else "F(CRP)"  
  
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
    rvpar$xmax = if(rvpar$axesInPercents) 100 else 1
    rvpar$ymax = if(rvpar$axesInPercents) 100 else 1
  
    ##----------------------------top viewport  
    pushViewport(vptop)
    setUpTrellisFn(nrow = 1, ncol = 1, main = rvpar$main,
                   xlab = rvpar$xlab, ylab = rvpar$ylab,
                   xmax = rvpar$xmax, ymax = rvpar$ymax, axesInPercents = rvpar$axesInPercents,
                   inflate = rvpar$inflate,
                   newpage = FALSE, atX = rvpar$atX, atY = rvpar$atY)
  
    ##----------------------------the action starts here
    nModels = length(crps)
    pch = rvpar$pch + rep(0,length(crps))
    cex = rvpar$cex + rep(0,length(crps))
    sapply(seq_along(crps), function(i) {
      riskCdfRawFn(pos = c(1, 1), 
        x1 = crps[[i]]$CRP, a1 = crps[[i]]$a,
        col = rvpar$col[i], 
        pch = pch[i], 
        cex = cex[i])
    })
    if(rvpar$legend){
      downViewport(paste(1, 1))
    
      legendvp = viewport(
        x = unit(1, "lines"),
        y = unit(1, "npc") - unit(1, "lines"),
        just = c("left", "top"),
        height = unit(nModels + 1, "lines"),
        width = max(stringWidth(names(crps))) + unit(3, "lines"),
        name = "legendvp")
      pushViewport(legendvp)
      grid.rect()
  
      grid.text(
        label = names(crps),
        x = unit(2, "lines"),
        y = unit(nModels:1, "lines"),
        just = "left")
 
      grid.points(
        x = unit(rep(1, nModels), "lines"),
        y = unit(nModels:1, "lines"),
        size = unit(cex, "char"), pch = pch, 
        gp = gpar(col = rvpar$col))
    }
    invisible(crps)
  }

# Wed Sep 21 20:55:44 PDT 2011
# Thu Sep 22 13:54:15 PDT 2011
# Change the order of y so the top one goes with the first model, etc.
# Tue Sep 27 14:48:36 PDT 2011
# Return crps.


