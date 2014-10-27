grouping_risk_fn = function(r, sim_params){
  KKK = sim_params$grouping_risk_K
  rMax = sim_params$grouping_risk_rMax
  list(
    variable = r,
    grouping = grouping_quantile_fn(r, KKK))
}

