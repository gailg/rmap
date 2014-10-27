

BlockDiagFn = function( boxes ) {
  # credit to simex package
  d.row = sapply(boxes, NROW)
  d.col = sapply(boxes, NCOL)
  d.diag = matrix(0, nrow = sum(d.row), ncol = sum(d.col))
  d.row = c(0, cumsum(d.row))
  d.col = c(0, cumsum(d.col))
  for (i in 1:length(boxes)) {
    d.diag[(d.row[i] + 1):d.row[i + 1], (d.col[i] + 1):d.col[i + 1]] = as.matrix(boxes[[i]])
  }
  d.diag
}

# Wed Mar 16 11:00:14 PDT 2011
