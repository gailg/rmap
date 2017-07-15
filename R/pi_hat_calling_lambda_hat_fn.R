pi_hat_calling_lambda_hat_fn = function(baseArgs = FALSE, extraArgs = FALSE) {
  lambdaHat = if("lambdaHat" %in% names(extraArgs)) extraArgs$lambdaHat else lambda_hat_fn(baseArgs)
  pi_hat = unlist(lapply(1:baseArgs$K, function(kkk) {
    lambdaDot = colSums( lambdaHat[[kkk]]$lambdaHat )
    sum(lambdaHat[[kkk]]$lambdaHat[1, ] * c(1, cumprod(1 - lambdaDot)[-length(lambdaDot)]))
  }))
  names(pi_hat) = paste0("k", 1:baseArgs$K)
  pi_hat
}