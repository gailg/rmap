

performanceDifference = function(e, t, rs, design = "randomSample", riskGroups, bootstrap = 100) {

  errorTxt = paste("'riskGroups' must be a named list with two elements.  The names of this list",
               "  must start with 'K', 'k', or 'cutoffs'.",
               "  Each element must hold a valid value for K, k, or cutoffs",
               "  (see ?riskValidateExternalFn)", sep = "\n")
  
  if(!is.numeric(bootstrap)) stop("'bootstrap' must be an integer.")
  
  if(!is.list(riskGroups) || length(riskGroups) != 2) {
    stop(errorTxt)
  }


  names(riskGroups) = sapply(names(riskGroups), function(name1) {
    if(substr(name1, 1, 1) %in% c("k", "K")) {
      substr(name1, 1, 1)
    } else if(substr(name1, 1, 7) == "cutoffs") {
      "cutoffs"
    } else {
      stop(errorTxt)
    }
  })
  
  baseArgs1 = baseArgsFn(e, t, rs$r1, design, riskGroups[1], rSummary = "mean", bootstrap)
  baseArgs2 = baseArgsFn(e, t, rs$r2, design, riskGroups[2], rSummary = "mean", bootstrap)
  performanceDifferenceInternalFn(baseArgs1, baseArgs2)
}

# Wed Mar 16 11:12:33 PDT 2011
