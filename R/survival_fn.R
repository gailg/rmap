survival_fn = function(t, survival_fit){
  sss = survival_fit$surv
  tau = survival_fit$time
  sapply(t, function(this_t){
    sss[max(which(tau <= this_t))]
  })
}