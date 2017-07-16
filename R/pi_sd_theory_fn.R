pi_sd_theory_fn = function(baseArgs, extraArgs){
  Sigma = SigmaFn(baseArgs, extraArgs)
  extraArgs$Sigma = Sigma
  sigma = sqrt(sapply(baseArgs$K:(2 * baseArgs$K - 1), function(kkk) Sigma[kkk, kkk]) / sum(baseArgs$N_two_stage))
  extraArgs$sigma = sigma
  piHatCIs = prob_CI_Fn(extraArgs$piHat, sigma)
  lower = piHatCIs[, "lower"]
  upper = piHatCIs[, "upper"]
  in_ci = ifelse(lower <= baseArgs$rSummary & baseArgs$rSummary <= upper, "yes", "no")
  list(
    pi_sd_theory = data.frame(
      sd = sigma,
      lower,
      upper,
      in_ci),
    extraArgs = extraArgs)
}