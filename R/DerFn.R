

DerFn = function(baseArgs = FALSE, extraArgs = FALSE) {
  lambdaHat = if("lambdaHat" %in% names(extraArgs)) extraArgs$lambdaHat else lambdaHatFn(baseArgs)

  lambdaBoxes = lapply( 1:baseArgs$K, function(kkk) {
    lambdaHat[[kkk]]$lambdaHat
  })

  DerBoxes = lapply(seq_len(baseArgs$K), function(kkk) {
    lambdaBoxes_k = lambdaBoxes[[kkk]]
    lambdaDot = colSums(lambdaBoxes_k)
    num = cumprod(1 - lambdaDot)
    MMM = length(num)
    Der2 = -sapply(seq_len(MMM), function(mmm) {
      if(mmm == MMM) {  
        0
      } else {
        sum(sapply((mmm+1):MMM, function(mPrime) {
          lambdaBoxes_k[1, mPrime] * num[mPrime - 1] / (1 - lambdaDot[mmm])
        }))
      }
    })
    Der1 = c(1, num[-length(num)]) + Der2
    matrix(c(Der1, Der2), ncol = 1)
  })

  Der_lambda = BlockDiagFn(DerBoxes)
  Der = if(baseArgs$K > 1) {
    BlockDiagFn(list(diag(rep(1, baseArgs$K - 1)), Der_lambda))
  } else {
    Der_lambda
  }

  Der
}

# Wed Mar 16 11:03:38 PDT 2011
