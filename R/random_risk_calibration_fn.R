random_risk_calibration_fn = function(gen_params){
  eta_mortality = gen_params$eta_mortality
  eta_biased_mortality =  gen_params$eta_biased_mortality
  N_subjects = gen_params$N_subjects
  tStar = gen_params$tStar
  
  eta_biased_outcome = with(gen_params,
    exp(rnorm(n = N_subjects, mean = mu_biased, sd = sigma_biased)))
  eta_missing_outcome = with(gen_params,
    exp(rnorm(n = N_subjects, mean = mu_missing, sd = sigma_missing)))
  eta_outcome = eta_biased_outcome * eta_missing_outcome
  
  r_correct = r_fn(eta_outcome, eta_mortality, tStar)
  r_biased_outcome = r_fn(eta_biased_outcome, eta_mortality, tStar)
  r_biased_mortality = r_fn(eta_outcome, eta_biased_mortality, tStar)
  r_biased_both = r_fn(eta_biased_outcome, eta_biased_mortality, tStar)
  
  tEvents = cbind(
    tCensor0 = rep(tStar, N_subjects),
    tCensor = pmin(rexp(N_subjects, gen_params$eta_censor), tStar),
    tDisease = rexp(N_subjects, eta_outcome),
    tDeath = if(eta_mortality == 0) tStar + 1 else rexp(N_subjects, eta_mortality))
  teCensoringOff = t(apply(
    tEvents[, c("tCensor0", "tDisease", "tDeath")], 1, function(row){
      c(min(row), which.min(row) - 1)
    }))
  eCensoringOff = teCensoringOff[,2]
  tCensoringOff = teCensoringOff[,1]
  yCensoringOff = as.integer(eCensoringOff == 1)
  te = t(apply(
    tEvents[, c("tCensor", "tDisease", "tDeath")], 1, function(row){
      c(min(row), which.min(row) - 1)
    }))
  e = te[,2]
  t = te[,1]
  y = as.integer(e == 1)
  e_outcome = as.numeric(e == 1)
  e_mortality = as.numeric(e == 2)
  Lambda_correct_outcome = eta_outcome * t
  Lambda_biased_outcome = eta_biased_outcome * t
  Lambda_correct_mortality = eta_mortality * t
  Lambda_biased_mortality = eta_biased_mortality * t
  if(!gen_params$censoring_on){
    data.frame(e = eCensoringOff, t = tCensoringOff, y = yCensoringOff,
               eta_outcome, eta_biased_outcome, eta_missing_outcome, 
               r_correct, r_biased_outcome, r_biased_mortality, r_biased_both,
               e_outcome, e_mortality,
               Lambda_correct_outcome, Lambda_biased_outcome,
               Lambda_correct_mortality, Lambda_biased_mortality)
  } else {
    data.frame(e, t, 
               eta_outcome, eta_biased_outcome, eta_missing_outcome, 
               r_correct, r_biased_outcome, r_biased_mortality, r_biased_both,
               e_outcome, e_mortality,
               Lambda_correct_outcome, Lambda_biased_outcome,
               Lambda_correct_mortality, Lambda_biased_mortality)
  }
}
