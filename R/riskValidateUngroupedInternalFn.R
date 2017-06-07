

riskValidateUngroupedInternalFn = function(baseArgs = FALSE, extraArgs = FALSE) {
  e = baseArgs$e
  t = baseArgs$t
  tStar = baseArgs$tStar
  baseArgs$e = ifelse(t > tStar, 0, e)
  baseArgs$t = ifelse(t > tStar, tStar, t)
  pnn = piHatNNInternalFn(baseArgs = baseArgs, extraArgs = extraArgs)
  rvu = list(PNN = pnn)

  class(rvu) = c("rvu", class(rvu))

  rvu
}
