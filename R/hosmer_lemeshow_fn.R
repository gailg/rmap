hosmer_lemeshow_fn = function(data, grouping){
  baseArgs = with(data, baseArgsFn(
    e, t, r, 
    design = "randomSample", 
    riskGroup = list(k = grouping), 
    rSummary = "mean", 
    bootstrap = FALSE))
  if(length(baseArgs$offendingRGs) == 0){
    gammaHat = gammaHatFn(baseArgs)
    lambdaHat = lambdaHatFn(baseArgs)
    extraArgs = list(gammaHat = gammaHat, lambdaHat = lambdaHat)
    piHat = piHatFn(baseArgs, extraArgs)
    extraArgs$piHat = piHat
    Sigma = SigmaFn(baseArgs, extraArgs)
    extraArgs$Sigma = Sigma
    piHatSummary = piHatSummaryFn(baseArgs, extraArgs)
    ChiSq = ChiSqFn(baseArgs, extraArgs)
    list(piHatSummary = piHatSummary,
         ChiSq = ChiSq)
  } else {
    list(piHatSummary = NA, 
         ChiSq = c(HosmerLemeshow = NA, HL_pval = NA))
  }
}
