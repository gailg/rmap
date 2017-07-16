gof_fn = function(baseArgs, gamma_hat, pi_hat, sigma) {
  gof_statistic = sum( gamma_hat * (pi_hat - baseArgs$rSummary)^2 / sigma^2 )
  p_value =  davies(gof_statistic, lambda = gamma_hat)$Qq
  c(statistic = gof_statistic, p_value = p_value)
}