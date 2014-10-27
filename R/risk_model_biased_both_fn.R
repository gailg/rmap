risk_model_biased_both_fn = function(ddd, sim_params){
  with(ddd, list(
    data = cbind(ddd[, c("e", "t", "e_outcome", "e_mortality")],
      e_combined = e_outcome + e_mortality,
      Lambda_outcome = Lambda_biased_outcome,
      Lambda_mortality = Lambda_biased_mortality,
      Lambda_mortality_den = Lambda_biased_mortality,
      Lambda_combined = Lambda_biased_outcome + Lambda_biased_mortality,
      Lambda_combined_den = Lambda_biased_outcome + Lambda_biased_mortality,
      r = r_biased_both),
    groupings = list(
      risk = grouping_risk_fn(r_biased_both, sim_params),
      rounded = grouping_rounded_fn(r_biased_both, sim_params),
      missing_outcome = grouping_missing_outcome_fn(
        eta_missing_outcome, sim_params))))    
}  
