percentile_ci_fn = function(baseArgs, concordance_boo){
  alpha = 1 - baseArgs$confidence_level
  ci = quantile(concordance_boo, c(alpha/2, 1 - alpha/2))
  names(ci) = c("lower", "upper")
  ci
}