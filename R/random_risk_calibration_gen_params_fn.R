random_risk_calibration_gen_params_fn = function(
  N_subjects, 
  censoring_on = TRUE,
  eta_censor = 0.056, 
  eta_outcome = 0.0035, 
  eta_mortality = 0.0042, 
  eta_biased_mortality = 0.0084,
  mu_missing = 0.5, 
  sigma_biased = 0.80, 
  sigma_missing = 0.75
){
  tStar = 10
  mu_biased = log(eta_outcome) - mu_missing
  gen_params = list( 
    modelName = "Quante",
    censoring_on = censoring_on,
    N_subjects = N_subjects,
    tStar = tStar,
    eta_censor = eta_censor,
    eta_mortality = eta_mortality,
    eta_biased_mortality = eta_biased_mortality,
    mu_biased = mu_biased,
    sigma_biased = sigma_biased,
    mu_missing = mu_missing,
    sigma_missing = sigma_missing
  )
}
