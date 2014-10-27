risk_model_uni_sim_fn = function(gen_params, sim_params){
  ddd = random_risk_calibration_fn(gen_params)
  correct = risk_model_correct_fn(ddd, sim_params)
  biased_outcome = risk_model_biased_outcome_fn(ddd, sim_params)
  biased_mortality = risk_model_biased_mortality_fn(ddd, sim_params)
  biased_both = risk_model_biased_both_fn(ddd, sim_params)
  risk_model_uni = list(
    correct = correct, 
    biased_outcome = biased_outcome,
    biased_mortality = biased_mortality,
    biased_both = biased_both)
  risk_model_uni
}
