

riskValidateUngroupedExternalFn = function(
    e, t, r, design, riskGroup, bootstrap,
    multicore, verbose) {

  rSummary = "mean"
  baseArgs = baseArgsFn(e = e, t = t, r = r, design = design,
    riskGroup = riskGroup, rSummary = rSummary, bootstrap = bootstrap,
    multicore = multicore, verbose = verbose)
  
  riskValidateUngroupedInternalFn(baseArgs = baseArgs)
}

# Sat Sep 10 09:02:21 PDT 2011
# Replace epsilon and tStar with riskGroup.


