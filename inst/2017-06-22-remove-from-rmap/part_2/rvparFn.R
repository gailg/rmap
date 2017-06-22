# We want to
# 1) remove xymax from the graphing options
# 2) allow the user to specify "lightCol"
#    if the user does not specify lightCol, it can get computed using colorRampPalette()


rvparFn = function(...) {

  args = list(...)

  #--------------------------------------------------- defining parameters
  rvpar = vector("list", 0)
  
  rvpar$col = if("col" %in% names(args)) {
    args$col
  } else {
    c("#B23AEE", "#006400", "#00CED1", "#FF3030")
  }
  
  rvpar$lightCol = if("lightCol" %in% names(args)) {
    args$lightCol
  } else {
    sapply(rvpar$col, function(col1) {
      colorRampPalette(colors = c(col1, "white"))(10)[8]
    })
  }

  defaults = list(
    annotate = TRUE,
    atX = NULL,
    atY = NULL,
    axesInPercents = TRUE,
    cex = 0.7,
    comment = NULL,
    inflate = 1.1,
    legend = TRUE,
    main = NULL,
    pch = 19,
    xlab = NULL,
    xmax = NULL,
    ylab = NULL,
    ymax = NULL
    )
  
  for(spec in names(defaults)) {
    rvpar = `[<-`(rvpar, spec,
                   if(spec %in% names(args)) args[spec] else defaults[spec])
  }

  #--------------------------------------------------- error checking on rvpar
  ##------------------------ col and lightCol
  if( all( c("col", "lightCol") %in% names(args))) {
    if(length(args$col) != length(args$lightCol))
      stop("'col' and 'lightCol' must be same-length vectors of colors.")
  }
  ##------------------------ annotate
  if(length(rvpar$annotate) != 1 || !is.logical(rvpar$annotate)) {
    stop("Please enter either TRUE or FALSE for annotate")
  }
  ##------------------------ axesInPercents
  if(! is.logical(rvpar$axesInPercents))
    stop("axesInPercents must be a logical value.")
  ##------------------------ cex
  ## let the graphics error messages do their thing
  ##------------------------ comment 
  if(!is.null(rvpar$comment) &&
     (length(rvpar$comment) != 1 || !is.character(rvpar$comment))
  ) {
    stop("Please enter either NULL or a valid character string for comment")
  }
  ##------------------------ inflate
  if( (rvpar$inflate < 1) || (rvpar$inflate > 1.5) ) {
    stop("inflate should be in [1, 1.5]")
  }
  ##------------------------ legend
  if(! is.logical(rvpar$legend))
    stop("legend must be a logical value.")
  ##------------------------ xmax and ymax
  if((!rvpar$axesInPercents) &&
     ((!is.null(rvpar$xmax)) && ((rvpar$xmax <= 0) || (rvpar$xmax > 1)))
  ) {
    stop("Since axesInPercents is FALSE, xmax and ymax should be in (0, 1]")
  }
  if((rvpar$axesInPercents) &&
     ((!is.null(rvpar$xmax)) && ((rvpar$xmax <= 0) || (rvpar$xmax > 100)))
  ) {
    stop("Since axesInPercents is TRUE, xmax and ymax should be in (0, 100]")
  }
  ##------------------------ pch
  ## let the graphics error messages do their thing

  if(! all(names(args) %in% c("col", "lightCol", names(defaults))) ) {
    warning(paste(
      "You gave arguments to rvpar other than",
      "the following arguments and will be ignored.",
      "'annotate', 'atX', 'atY', 'axesInPercents'",
      "'cex', 'comment', 'inflate', 'legend', 'main'",
      "'pch', 'xlab', 'xmax', 'ylab', 'ymax'.",
      sep = "\n"))  
  }
  class(rvpar) = c("rvpar", class(rvpar))
  rvpar
}

# Wed Aug 24 12:40:14 PDT 2011
# Wed Sep 21 20:34:49 PDT 2011  legend added and everything cleaned up a little.



