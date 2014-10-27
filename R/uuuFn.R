

uuuFn = function(baseArgs = FALSE, extraArgs = FALSE) {
  gammaHat = if("gammaHat" %in% names(extraArgs)) extraArgs$gammaHat else gammaHatFn(baseArgs)
  lambdaHat = if("lambdaHat" %in% names(extraArgs)) extraArgs$lambdaHat else lambdaHatFn(baseArgs)
  eventUni = 0:2
  
  uuuGamma = sapply(1:(baseArgs$K - 1), function(kkk) {
    (baseArgs$k[order(baseArgs$k)] == kkk) / gammaHat[kkk] - (baseArgs$k[order(baseArgs$k)] == baseArgs$K) / gammaHat[baseArgs$K] ###DJDJ
  })
                                        # Had to reorder based on the order of $k to get results consistent with rime 0.00.008

  uuuLamba = BlockDiagFn(lapply(1:baseArgs$K, function(kkk) {
    do.call(cbind, structure(lapply(eventUni[-1], function(eee) {
      
      tau_k = lambdaHat[[kkk]]$tau
      DDD_k = lambdaHat[[kkk]]$DDD
      lambdaHat_k = lambdaHat[[kkk]]$lambdaHat
      NAR_k = lambdaHat[[kkk]]$NAR
      
      term1 = sapply(seq_along(tau_k), function(m) DDD_k[[eee]][,m] / lambdaHat_k[eee,m])
      term1 = ifelse(is.nan(term1), 0, term1)
      term2 = sapply(seq_along(tau_k), function(m) (!DDD_k[[eee]][,m]) / (1 - lambdaHat_k[eee,m]) )
      term2 = sapply(seq_along(tau_k), function(m)
        (1-DDD_k[[1]][,m]-DDD_k[[2]][,m]) / (1 - lambdaHat_k[1,m]- lambdaHat_k[2,m]) )
      term2 = ifelse(is.nan(term2) | is.infinite(term2), 0, term2)
      NAR_k * (term1 - term2)
      
    }), .Names = paste("e", eventUni[-1], sep = "") ))
  }))

  uuu = if(baseArgs$K > 1) {
    cbind(uuuGamma, uuuLamba)
  } else {
    uuuLamba
  }
  uuu
}

# Wed Mar 16 11:01:13 PDT 2011
