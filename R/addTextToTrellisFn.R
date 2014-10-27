

addTextToTrellisFn = function(pos, text, where = "topleft", ...) {

  if( ! grepl("layoutVp", as.character(current.vpTree()))) {
    stop("setUpTrellisFn() must be called before this function.")
  }

  downViewport(paste(pos, collapse = " "))
  on.exit(upViewport(0))

  whereError = "where must be 'topleft', 'topright', bottomleft', or 'bottomright'."
  just = character(2)
  
  if(where %in% c("topleft", "topright")) {
    y = unit(1, "npc") - unit(0.5, "lines")
    just[2] = "top"
  } else if (where %in% c("bottomleft", "bottomright")) {
    y = unit(0.5, "lines")
    just[2] = "bottom"
  } else {
    stop(whereError)
  }

  if(where %in% c("topleft", "bottomleft")) {
    x = unit(0.5, "lines")
    just[1] = "left"
  } else if(where %in% c("topright", "bottomright")) {
    x = unit(1, "npc") - unit(0.5, "lines")
    just[1] = "right"
  }

  grid.text(text, x, y, just, ...)

  invisible(text)
}
