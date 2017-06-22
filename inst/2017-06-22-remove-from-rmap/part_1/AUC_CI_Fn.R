# Will go in package.

AUC_CI_Fn = function(baseArgs = FALSE, extraArgs = FALSE) {

  if(baseArgs$ungrouped){

    baseArgs$nBootstraps = max(200, baseArgs$nBootstraps)
    
    auc = if( "auc" %in% names(extraArgs) ) extraArgs$auc else AUC_Fn(baseArgs, extraArgs)
    bootReps = sapply(seq_len(baseArgs$nBootstraps), function(i){
      baseArgsBoot = baseArgsBootFn(baseArgs)
      AUC_Fn(baseArgsBoot)
    })
    sdBoot = sd(bootReps)
    prob_CI_Fn(auc, sdBoot)[1,]
  } else {
    if(baseArgs$K == 1) {
      "AUC does not apply for K = 1"
    } else {
      piHat = if("piHat" %in% names(extraArgs)) extraArgs$piHat else piHatFn(baseArgs, extraArgs)
      gammaHat = if("gammaHat" %in% names(extraArgs)) extraArgs$gammaHat else gammaHatFn(baseArgs)
      Sigma = if("Sigma" %in% names(extraArgs)) extraArgs$Sigma else SigmaFn(baseArgs, extraArgs)
      
                                        # <appears in AUC_Fn too>
      f1 = sum( gammaHat^2 * ( 1 - piHat) * piHat)
      f2 = sum(unlist(sapply(1:(baseArgs$K - 1), function(k1) {
        sapply((k1 + 1):baseArgs$K, function(k2) {
          gammaHat[k1] * gammaHat[k2] * (1 - piHat[k1]) * piHat[k2]
        })
      })))
      pih = sum(gammaHat * piHat)
      ggg = (1 - pih) * pih
      AUC = (f1 / 2 + f2) / ggg
                                        # </appears>
      
      
      work = gammaHat * ( 1 - piHat) * piHat
      Df1Dgamma = 2 * ( work[-length(work)] - work[length(work)])
      Df1Dpi = gammaHat^2 * (1 - 2 * piHat)
      Df1 = c(Df1Dgamma, Df1Dpi)
      term3 = sum((gammaHat * (1 - piHat))[-baseArgs$K] * piHat[baseArgs$K])
      seq_fn = function(i, j) if(i > j) integer(0) else i:j 
      Df2Dgamma = sapply(1:(baseArgs$K - 1), function(kkk) {
        term1 = sum(unlist(sapply(seq_fn(1, kkk - 1), function(k1) gammaHat[k1] * ( 1 - piHat[k1]) * piHat[kkk] )))
        term2 = sum(sapply(seq_fn(kkk + 1, baseArgs$K), function(k2) gammaHat[k2] * (1 - piHat[kkk]) * piHat[k2]))
        term1 + term2 - term3
      })  
      Df2Dpi = sapply(1:baseArgs$K, function(kkk) {
        term1 = sum(unlist(sapply(seq_fn(1, kkk - 1), function(k1) gammaHat[k1] * gammaHat[kkk] * ( 1 - piHat[k1]) )))
        term2 = sum(unlist(sapply(seq_fn(kkk + 1, baseArgs$K), function(k2) gammaHat[kkk] * gammaHat[k2] * piHat[k2]) ))
        term1 - term2
      })
      Df2 = c(Df2Dgamma, Df2Dpi)
      DgDgamma = (1 - 2 * pih) * (piHat[-length(piHat)] - piHat[length(piHat)])
      DgDpi = (1 - 2 * pih) * gammaHat
      Dg = c(DgDgamma, DgDpi)
      DA = ((Df1/2 + Df2) * ggg - (f1/2 + f2) * Dg) / ggg ^ 2
      VarAUC = DA %*% Sigma %*% DA / sum(baseArgs$N)
      
      prob_CI_Fn(AUC, sqrt(VarAUC))[1,]
    }
  }
}

# Tue Aug 23 11:40:45 PDT 2011

