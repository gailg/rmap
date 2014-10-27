risk_model_correct_fn = function(ddd, sim_params){
  with(ddd, list(
    data = cbind(ddd[, c("e", "t", "e_outcome", "e_mortality")],
      e_combined = e_outcome + e_mortality,
      Lambda_outcome = Lambda_correct_outcome,
      Lambda_mortality = Lambda_correct_mortality,
      Lambda_mortality_den = Lambda_correct_mortality,
      Lambda_combined = Lambda_correct_outcome + Lambda_correct_mortality,
      Lambda_combined_den = Lambda_correct_outcome + Lambda_correct_mortality,
      r = r_correct),
    groupings = list(
      risk = grouping_risk_fn(r_correct, sim_params),
      rounded = grouping_rounded_fn(r_correct, sim_params),
      missing_outcome = grouping_missing_outcome_fn(
        eta_missing_outcome, sim_params))))
}
  







  
