

SigmaFn = function(baseArgs = FALSE, extraArgs = FALSE) {
  Der = if("Der" %in% names(extraArgs)) extraArgs$Der else DerFn(baseArgs, extraArgs)
  V2Stage = if("V2Stage" %in% names(extraArgs)) extraArgs$V2Stage else V2StageFn(baseArgs, extraArgs)
  t(Der) %*% V2Stage %*% Der
}

# Wed Mar 16 11:04:00 PDT 2011
