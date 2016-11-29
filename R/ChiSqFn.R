

ChiSqFn = function(baseArgs = FALSE, extraArgs = FALSE) {
  piHat = if("piHat" %in% names(extraArgs)) extraArgs$piHat else piHatFn(baseArgs, extraArgs)
  Sigma = if("Sigma" %in% names(extraArgs)) extraArgs$Sigma else SigmaFn(baseArgs, extraArgs)

  yyy = piHat - baseArgs$rSummary
  SigmaPi = Sigma[baseArgs$K:(2 * baseArgs$K - 1), baseArgs$K:(2 * baseArgs$K - 1)]

  #<<< DJ adding for v0.01-01 Thu May 12 09:18:20 PDT 2011
  if(baseArgs$K == 1) SigmaPi = as.matrix(SigmaPi)
  #>>>

  ## Pearson = sum(baseArgs$N) * yyy %*% solve(SigmaPi) %*% yyy
  ## Pearson_pval = 1 - pchisq(Pearson, baseArgs$K)

  sigma = sqrt(sapply(1:baseArgs$K, function(kkk) SigmaPi[kkk, kkk]) / sum(baseArgs$N))
  HosmerLemeshow = sum((piHat - baseArgs$rSummary)^2 / sigma^2 )
  gammaHat = extraArgs$gammaHat
  HosmerLemeshow = sum( gammaHat * (piHat - baseArgs$rSummary)^2 / sigma^2 )
  HL_pval =  davies(HosmerLemeshow, lambda = extraArgs$gammaHat)$Qq

  ## c(Pearson = Pearson, Pearson_pval = Pearson_pval,
  ##   HosmerLemeshow = HosmerLemeshow, HL_pval = HL_pval)
  c(HosmerLemeshow = HosmerLemeshow, HL_pval = HL_pval)
}

# Wed Mar 16 11:05:14 PDT 2011
