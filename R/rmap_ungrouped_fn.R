rmap_ungrouped_fn = function(baseArgs){
  e = baseArgs$e
  t = baseArgs$t
  tStar = baseArgs$tStar
  baseArgs$e = ifelse(t > tStar, 0, e)
  baseArgs$t = ifelse(t > tStar, tStar, t)
  estimate = pi_hat_nn_fn(baseArgs)
  confidence_band = if(baseArgs$N_bootstraps == 0){
    NULL
  } else {
    iter_fn = if(baseArgs$multicore) {
      require(parallel)
      mclapply
    } else {
      lapply
    }
    randomSeeds = sample(1:1e8, baseArgs$N_bootstraps, replace = FALSE)
    bootstraps_raw = iter_fn(seq_len(baseArgs$N_bootstraps), function(n_bootstrap) {
      set.seed(randomSeeds[n_bootstrap])
      if(baseArgs$verbose) 
        print(paste("PID: ", Sys.getpid(), " ", date(),
                    " rmap_ungrouped_fn: starting bootstrap ", n_bootstrap, sep = "")) 
      while(TRUE) {
        baseArgsBoot = baseArgsBootFn(baseArgs)
        pi_hat_nn_try = tryCatch(pi_hat_nn_fn(baseArgsBoot),
                                 error = function(pi_hat_nn_try) "error!")
        if( !(is.character(pi_hat_nn_try) && pi_hat_nn_try == "error!") ) {
          break
        } else {
          if(baseArgs$verbose)
            print(paste("PID: ", Sys.getpid(), " ", date(),
                        " rmap_ungrouped_fn: BAD SAMPLE in bootstrap ",
                        n_bootstrap, ". RESAMPLING.", sep = ""))
        }
      }
      pi_hat_nn_try      
    })
    bootstraps = gather_fn(bootstraps_raw, all_rho = estimate[, "rho"])
    bootstraps_interpolated = t(apply(bootstraps, 1, interpolateOneBsFn, estimate[, "rho"]))
    confidence_level = baseArgs$confidence_level
    prob_lower = (1 - confidence_level)/2
    prob_upper = 1 - prob_lower
    confidence_band_0 = data.frame(t(apply(
      bootstraps_interpolated, 2, quantile, probs = c(prob_lower, prob_upper))))
    row.names(confidence_band_0) = NULL
    names(confidence_band_0) = c("lower", "upper")
    confidence_band_0
  }
  row.names(estimate) = NULL
  names(estimate) = c("assigned_risk", "observed_risk")
  if(is.null(confidence_band)){
    estimate
  } else {
    cbind(estimate, confidence_band)
  }
}