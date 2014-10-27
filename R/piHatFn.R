

piHatFn = function(baseArgs = FALSE, extraArgs = FALSE) {
  lambdaHat = if("lambdaHat" %in% names(extraArgs)) extraArgs$lambdaHat else lambdaHatFn(baseArgs)
  
  structure(sapply(1:baseArgs$K, function(kkk) {
    lambdaDot = colSums( lambdaHat[[kkk]]$lambdaHat )
    sum(lambdaHat[[kkk]]$lambdaHat[1, ] * c(1, cumprod(1 - lambdaDot)[-length(lambdaDot)]))
  }), .Names = paste("k", 1:baseArgs$K, sep = ""))
}

# Wed Mar 16 10:59:17 PDT 2011
