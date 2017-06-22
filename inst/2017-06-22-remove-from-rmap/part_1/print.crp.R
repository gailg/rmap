print.crp = function(crp, n = 6, ...){
  print.default(crp[!names(crp) %in% c("crp", "crp1", "crp2")])
  if("crp1" %in% names(crp)){
    cat("\nHead of Model 1 caseRiskPercentiles: \n")
    print.default(head(crp$crp1, n = n))
    cat("\nHead of Model 2 caseRiskPercentiles: \n")
    print.default(head(crp$crp2, n = n))
  } else {
    cat("\nHead ofcaseRiskPercentiles: \n")
    print.default(head(crp$crp, n = n))
  }
  invisible(crp)
}
