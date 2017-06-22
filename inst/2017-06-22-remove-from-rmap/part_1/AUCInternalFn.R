

AUCInternalFn = function(baseArgs = FALSE, extraArgs = FALSE) {

  # 0. Compute CRP, send it as extraArg to AUC_Fn
  crp = CRP_Fn(baseArgs, extraArgs = extraArgs)
  
  # 1. Call AUC_Fn
  auc = AUC_Fn(baseArgs = baseArgs, extraArgs = list(crp = crp))

  # 2. Call AUC_CI_Fn
  aucCI = AUC_CI_Fn(baseArgs = baseArgs, extraArgs = list(auc = auc))
  list(AUC = auc, AUC_CI = aucCI, CRP = crp)
  
}
