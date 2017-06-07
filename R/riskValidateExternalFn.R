

riskValidateExternalFn = function(e, t, r, t_star, design = "randomSample", riskGroup, rSummary, bootstrap = FALSE) {
  baseArgs = baseArgsFn(e, t, r, t_star, design, riskGroup, rSummary, bootstrap)
  riskValidateInternalFn(baseArgs)
}

# Wed Mar 16 11:11:50 PDT 2011
