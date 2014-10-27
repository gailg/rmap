

riskValidateUngroupedInternalFn = function(baseArgs = FALSE, extraArgs = FALSE) {

  auc = AUCInternalFn(baseArgs = baseArgs, extraArgs = extraArgs)
  # give us AUC, AUC_CI, and CRP

  pnn = piHatNNInternalFn(baseArgs = baseArgs, extraArgs = extraArgs)

  rvu = list(AUC = auc, PNN = pnn)

  class(rvu) = c("rvu", class(rvu))

  rvu
}
