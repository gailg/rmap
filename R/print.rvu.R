

print.rvu = function(rvu, n = 6, ...) {

  cat("AUC: \n")
  print.default(rvu$AUC$AUC)

  cat("\nAUC_CI: \n")
  print.default(rvu$AUC$AUC_CI)

  cat("\nHead of CRP's: \n")
  print.default(head(rvu$AUC$CRP$CRP, n = n))

  cat("\nHead of Nearest Neighbor piHat estimates: \n")
  print(head(rvu$PNN$est, n = n))

  cat("\nHead of Nearest Neighbor piHat bootstrap 95% confidence band: \n")
  print.default(head(rvu$PNN$confBand, n = n))

  invisible(rvu)
}
