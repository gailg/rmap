concordance_estimate_fn = function(baseArgs){
  concordance = concordance_fn(baseArgs)
  roc = concordance$roc
  concordance = concordance$concordance
  sensitivity = roc$sensitivity
  specificity = roc$specificity
  one_minus_specificity = 1 - specificity
  roc_df_0 = unique(data.frame(one_minus_specificity, sensitivity))
  df_for_roc_plot = roc_df_0[order(roc_df_0$one_minus_specificity, roc_df_0$sensitivity), ]
  list(concordance = concordance,
       df_for_roc_plot = df_for_roc_plot)
}