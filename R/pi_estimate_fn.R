pi_estimate_fn = function(baseArgs){
  gammaHat = gammaHatFn(baseArgs)
  piHat = pi_hat_fn(baseArgs)
  r_bar = baseArgs$rSummary
  list(
    pi_estimate = data.frame(
      gamma_hat = gammaHat,
      r = baseArgs$rSummary,
      pi_hat = piHat),
    extraArgs = NULL)
}