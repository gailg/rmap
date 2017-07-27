rmap_simplest_example = function(e, t, r, t_star, K = 4, N_bootstraps = 100){
  design = "random_sample"
  risk_group = list(K = K)
  r_summary = "mean"
  confidence_level = 0.95
  N_cores = 1
  verbose = FALSE
  grouped = rmap(e, t, r, t_star, design, risk_group, r_summary, 
                 N_bootstraps, confidence_level, N_cores, verbose)
  numerical_summaries = grouped$numerical_summaries
  #----------------------------------------------------------- concordance and roc
  concordance_annotation_just_estimate = 
    paste0("Concordance = ",
           round(numerical_summaries$concordance["estimate"] * 100, 1),
           "%")
  concordance_annotation = if("lower" %in% names(numerical_summaries$concordance)){
    paste0(concordance_annotation_just_estimate,
           ", CI = (",
           round(numerical_summaries$concordance["lower"] * 100, 1),
           "%, ",
           round(numerical_summaries$concordance["upper"] * 100, 1), 
           "%)")
  } else {
    concordance_annotation_just_estimate
  }
  roc_plot = grouped$plot$roc_plot +
    xlab("1 - Specificity") + ylab("Sensitivity") +
    labs(title = "ROC Plot",
         subtitle = concordance_annotation)
  #-------------------------------------------------------------------- risk and gof
  risk_annotation = paste0("GOF = ", 
                           round(numerical_summaries$gof_asymptotic["statistic"], 4),
                           ", P-Value = ", 
                           round(numerical_summaries$gof_asymptotic["p_value"], 4))
  grouped_risk_plot = grouped$plots$risk_plot +
    labs(title = "Grouped Attribute Diagram",
         subtitle = risk_annotation)
  list(roc_plot = roc_plot,
       grouped_risk_plot = grouped_risk_plot)
}