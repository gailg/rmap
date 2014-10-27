grouping_missing_outcome_fn = function(eta_missing_outcome, sim_params){
  KKK = sim_params$grouping_missing_outcome_K
  xxx = eta_missing_outcome
  xMax = sim_params$grouping_missing_outcome_xMax
  list(
    variable = eta_missing_outcome,
    grouping = grouping_quantile_fn(eta_missing_outcome, KKK))
}

