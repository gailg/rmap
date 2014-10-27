

piHatSummaryFn = function(baseArgs = FALSE, extraArgs = FALSE) {
  piHat = if("piHat" %in% names(extraArgs)) extraArgs$piHat else piHatFn(baseArgs, extraArgs)
  Sigma = if("Sigma" %in% names(extraArgs)) extraArgs$Sigma else SigmaFn(baseArgs, extraArgs)

  sigma = sqrt(sapply(baseArgs$K:(2 * baseArgs$K - 1), function(kkk) Sigma[kkk, kkk]) / sum(baseArgs$N))

  piHatCIs = prob_CI_Fn(piHat, sigma)
  
  piHatSummary = data.frame( r = baseArgs$rSummary, piHat = piHat, sigma = sigma,
    lower = piHatCIs[, "lower"],
    upper = piHatCIs[, "upper"],
    inCI = ifelse(baseArgs$rSummary <= piHatCIs[, "upper"] & baseArgs$rSummary >= piHatCIs[, "lower"], "yes", "no"))

  piHatSummary
}

# Wed Mar 16 11:04:47 PDT 2011
