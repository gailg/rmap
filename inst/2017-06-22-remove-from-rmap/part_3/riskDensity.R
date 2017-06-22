

riskDensity = function( e, t, r, tStar, adjust = 1,
  rvpar = rvparFn(), ocall = NULL) {

  rvpar$axesInPercents = FALSE
  
  if (!"package:grid" %in% search()) {
    print("Loading 'grid' package")
    library(grid)
    print("Done loading 'grid' package")
  }
  if (is.null(ocall)) 
    ocall = deparse(match.call())

  ##----------------------------data checks
  vecs = list(e = e, t = t, r = r)
  if( ! all( sapply(vecs, is.numeric) ) )
    stop("e, t, and r must be numeric vectors.")

  if( length( unique( sapply(vecs, length))) != 1)
    stop("e, t, and r must all have the same length.")

  if(!all( vecs$e %in% c(0, 1, 2)))
    stop("every 'e' value must be 0, 1, or 2")

  ## convert non-hex lightCol to hex format:
  whichSpecial = which(substr(rvpar$lightCol, 1, 1) != "#")
  rvpar$lightCol[whichSpecial] = apply(
    col2rgb(rvpar$lightCol[whichSpecial]),
    2,
    function(col) {
      rgb(col[1], col[2], col[3], 48, maxColorValue = 255)
    })

  ## change the hex colors that AREN'T already transparent
  ## TO a transparent version of that color
  rvpar$lightCol[nchar(rvpar$lightCol) == 7] = paste(
    rvpar$lightCol[nchar(rvpar$lightCol) == 7], "50", sep = "")

  if(is.null(rvpar$xlab)) rvpar$xlab = "Assigned Risks"
  if(is.null(rvpar$ylab)) rvpar$ylab = "Density"
  
  grid.newpage()
  vpbottom = viewport(
    x = unit(0.5, "cm"),
    y = unit(0.25, "cm"),
    width = unit(1, "npc") - unit(1, "cm"),
    height = unit(5, "lines") - unit(0.5, "cm"),
    just = c("left", "bottom"),
    name = "vpbottom", clip = "on")
  vptop = viewport(
    x = 0,
    y = unit(5, "lines"),
    width = unit(1, "npc"),
    height = unit(1, "npc") - unit(5, "lines"),
    just = c("left", "bottom"),
    name = "vptop")

  ##----------------------------bottom viewport
  pushViewport(vpbottom)
  on.exit(upViewport(0))
  
  grid.rect()
  currentDate = as.character(Sys.time())
  currentCall = ocall
  grid.text(
    paste(rvpar$comment, currentDate, currentCall, sep = "\n"),
    x = unit(1.5, "cm"),
    y = unit(0.5, "npc"), 
    just = c("left", "center"))

  upViewport()

  ##----------------------------xymax
  ## xmax:
  if(is.null(rvpar$xmax)) {
    rvpar$xmax = min(1, max(r) + 0.15)
  }

  ## ymax:
  if(is.null(rvpar$ymax)) {
    affected = r[e == 1]
    unaffected = r[ ((e == 0) & (t > (tStar - 0.01))) | (e == 2) ]
    
    dens_unaffected = density(
      unaffected, from = 0, to = 1, adjust = adjust)
    dens_affected = density(
      affected, from = 0, to = 1, adjust = adjust)
    rvpar$ymax = max( c(dens_unaffected$y, dens_affected$y) ) * 1.2
  }

  # browser()
  
  ##----------------------------top viewport

  pushViewport(vptop)
  setUpTrellisFn(
    nrow = 1,
    ncol = 1,
    main = rvpar$main,
    xlab = rvpar$xlab, ylab = rvpar$ylab,
    axesInPercents = rvpar$axesInPercents,
    xmax = rvpar$xmax, ymax = rvpar$ymax,
    inflate = rvpar$inflate,
    newpage = FALSE,
    atX = rvpar$atX, atY = rvpar$atY)

  riskDensityRawFn(
    pos = c(1, 1), e = e, t = t, r = r, tStar = tStar,
    col1 = rvpar$col[1], col2 = rvpar$col[2],
    lightCol1 = rvpar$lightCol[1], lightCol2 = rvpar$lightCol[2],
    adjust = adjust, legend = rvpar$legend)
}

# Wed Sep 21 20:46:44 PDT 2011
# legend set to rvpar$legend instead of hardwired to TRUE.
