rmap_grouped_fn = function(baseArgs){
  pi_estimate = pi_estimate_fn(baseArgs)$pi_estimate
  gamma_hat = pi_estimate$gamma_hat
  pi_hat = pi_estimate$pi_hat
  extraArgs = list(gammaHat = gamma_hat,
                   piHat = pi_hat)
  pi_sd_theory = if(any(baseArgs$N_nonzero_events == 0 || baseArgs$sampling == "weighted")){
    NULL
  } else {
    pi_sd_theory_fn(baseArgs, extraArgs)$pi_sd_theory
  }
  gof_theory = if(is.null(pi_sd_theory)){
    NULL
  } else {
    sigma = pi_sd_theory$sd
    gof_fn(baseArgs, gamma_hat, pi_hat, sigma)
  }
  ppp  = pi_sd_boot_fn(baseArgs)
  pi_sd_boot = ppp$pi_sd_boot
  concordance_ci = ppp$concordance_ci
  sigma = pi_sd_boot$sd_boot
  gof_bootstrap = if(any(pi_sd_boot$sd_boot < baseArgs$small_number)){
    NULL
  } else {
    gof_fn(baseArgs, gamma_hat, pi_hat, sigma)
  }
  concordance_estimate = concordance_estimate_fn(baseArgs)
  concordance_summary = c(concordance = concordance_estimate$concordance,
                          concordance_ci)
  df_for_roc_plot = concordance_estimate$df_for_roc_plot
  roc_plot = ggplot(df_for_roc_plot, aes(x = one_minus_specificity, y = sensitivity)) + 
    geom_step()
  sd_part = if(baseArgs$sampling == "weighted"){
    pi_sd_boot[, c("lower", "upper")]
  } else {
    pi_sd_theory[, c("lower", "upper")]
  }
  df_for_risk_plot = 100 * cbind(pi_estimate[, c("r", "pi_hat")], sd_part)
  risk_plot = ggplot(df_for_risk_plot, aes(x = r, y = pi_hat, ymin = lower, ymax = upper)) +
    geom_point() +
    geom_errorbar(aes(ymin = df_for_risk_plot$lower, ymax = df_for_risk_plot$upper), width = 2) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = 2) +
    xlim(0,100) + ylim(0, 100) +
    xlab("Assigned Risk (%)") + ylab("Observed Risk (%)")
  plots = list(
    df_for_roc_plot = concordance_estimate$df_for_roc_plot,
    roc_plot = roc_plot,
    risk_plot = risk_plot)
  summary = list(pi_estimate = pi_estimate,
                 pi_sd_theory = pi_sd_theory,
                 pi_sd_boot = pi_sd_boot,
                 gof_theory = gof_theory,
                 gof_bootstrap = gof_bootstrap,
                 concordance_summary = concordance_summary
  )
  list(plots = plots,
       summary = summary)
}