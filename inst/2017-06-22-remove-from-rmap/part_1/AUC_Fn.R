
# New package version.
# Will deal with ungrouped case

AUC_Fn = function(baseArgs = FALSE, extraArgs = FALSE) {
  if(baseArgs$ungrouped){
    crp = if("crp" %in% names(extraArgs) ) extraArgs$crp else CRP_Fn(baseArgs,extraArgs)
    auc = sum(crp$CRP * crp$a) / sum(crp$a)
    auc
  } else {
    if(baseArgs$K == 1) {
      "AUC does not apply for K = 1"
    } else {
      piHat = if("piHat" %in% names(extraArgs)) extraArgs$piHat else piHatFn(baseArgs, extraArgs)
      gammaHat = if("gammaHat" %in% names(extraArgs)) extraArgs$gammaHat else gammaHatFn(baseArgs)
      
                                        # <appears in AUC_CI_Fn too>
      f1 = sum( gammaHat^2 * ( 1 - piHat) * piHat)
      f2 = sum(unlist(sapply(1:(baseArgs$K - 1), function(k1) {
        sapply((k1 + 1):baseArgs$K, function(k2) {
          gammaHat[k1] * gammaHat[k2] * (1 - piHat[k1]) * piHat[k2]
        })
      })))
      pih = sum(gammaHat * piHat)
      ggg = (1 - pih) * pih
                                        # </appears>
      (f1 / 2 + f2) / ggg
    }
  }
}

# Wed Mar 16 11:05:38 PDT 2011
