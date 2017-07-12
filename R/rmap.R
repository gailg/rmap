rmap = function(e, t, r, tStar, design, riskGroup, rSummary, bootstrap, 
                confidenceLevel = 0.95, multicore = FALSE, verbose = FALSE){
  baseArgs = baseArgsFn(e, t, r, tStar, design, riskGroup, rSummary, bootstrap, 
                        confidenceLevel, multicore, verbose)
  if( baseArgs$ungrouped ) {
    df_for_risk_plot_0 = rmap_ungrouped_fn(baseArgs)
    df_for_risk_plot = 100 * df_for_risk_plot_0
    risk_plot = ggplot(df_for_risk_plot, aes(x = assigned_risk, y = observed_risk, ymin = lower, ymax = upper)) +
      geom_line() +
      geom_ribbon(alpha = 0.3) +
      geom_abline(slope = 1, intercept = 0, color = "red", linetype = 2) +
      xlim(0,100) + ylim(0, 100) +
      xlab("Assigned Risks (%)") + ylab("Observed Risks (%)")
    list(df_for_risk_plot = df_for_risk_plot,
         risk_plot = risk_plot)
  } else{
    rv = if(baseArgs$sampling == "weighted"){
      rmap_weighted_fn(baseArgs)
    } else {
      riskValidateInternalFn(baseArgs)
    }
    
    df_for_roc_plot = rv$df_for_roc_plot
    roc_plot = ggplot(df_for_roc_plot, aes(x = one_minus_specificity, y = sensitivity)) +
      geom_step() + 
      xlab("1 - Specificity") +
      ylab("Sensitivity")
    pi_summary = rv$pi_summary
    df_for_risk_plot = as.data.frame(100 * as.matrix(pi_summary[, c("r", "pi_hat", "lower", "upper")]))
    risk_plot = ggplot(df_for_risk_plot, aes(x = r, y = pi_hat, ymin = lower, ymax = upper)) +
      geom_point() +
      geom_errorbar(aes(ymin = df_for_risk_plot$lower, ymax = df_for_risk_plot$upper), width = 2) +
      geom_abline(slope = 1, intercept = 0, color = "red", linetype = 2) +
      xlim(0,100) + ylim(0, 100) +
      xlab("Assigned Risk (%)") + ylab("Observed Risk (%)")
    list(concordance_summary = rv$concordance_summary,
         df_for_roc_plot = df_for_roc_plot,
         roc_plot = roc_plot,
         gof = rv$gof,
         pi_summary = pi_summary,
         df_for_risk_plot = df_for_risk_plot,
         risk_plot = risk_plot)
  }
}
