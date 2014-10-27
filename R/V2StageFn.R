

V2StageFn = function(baseArgs = FALSE, extraArgs = FALSE) {
  VVV = if("VVV" %in% names(extraArgs)) extraArgs$VVV else VVVFn(baseArgs, extraArgs)
  if(!all(baseArgs$N == baseArgs$n)) {
    B2 = if("B2" %in% names(extraArgs)) extraArgs$B2 else B2Fn(baseArgs, extraArgs)
    VVV + VVV %*% B2 %*% VVV
  } else {
    VVV
  }
}

# Wed Mar 16 11:03:13 PDT 2011
