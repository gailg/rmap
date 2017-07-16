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
    bootstraps_raw = lapply(seq(from = 1, to = baseArgs$N_bootstraps, by = 1), function(n_bootstrap){
      if(baseArgs$verbose){
        print(paste("PID: ", Sys.getpid(), " ", date(),
                    " rmap_ungrouped_fn: starting bootstrap ", n_bootstrap, sep = "")) 
      }
      set.seed(randomSeeds[n_bootstrap])
      still_looking = TRUE
      while(still_looking){
        base_args_boot = base_args_boot_fn(baseArgs)
        base_args_boot$boot_code
        still_looking = !(base_args_boot$boot_code == 0)
      }
      pi_hat_nn_fn(base_args_boot)
    })
    bootstraps = gather_fn(bootstraps_raw, estimate[, "rho"])
    bootstraps_interpolated = t(apply(bootstraps, 1, interpolate_one_bootstrap_fn, estimate[, "rho"]))
    bootstraps_interpolated
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
    list(estimate = estimate)
  } else {
    df_for_risk_plot = 100 * cbind(estimate, confidence_band)
    risk_plot = ggplot(df_for_risk_plot, aes(x = assigned_risk, y = observed_risk, ymin = lower, ymax = upper)) +
      geom_line() +
      geom_ribbon(alpha = 0.3) +
      geom_abline(slope = 1, intercept = 0, color = "red", linetype = 2) +
      xlim(0, 100) + ylim(0, 100) +
      xlab("Assigned Risks (%)") + ylab("Observed Risks (%)")
    list(df_for_risk_plot = df_for_risk_plot,
         risk_plot = risk_plot)
  }
}