

riskValidateExternalFn = function(e, t, r, tStar, design = "randomSample", riskGroup, rSummary, bootstrap = FALSE) {
  baseArgs = baseArgsFn(e, t, r, tStar, design, riskGroup, rSummary, bootstrap)
  riskValidateInternalFn(baseArgs)
}

# Wed Mar 16 11:11:50 PDT 2011
