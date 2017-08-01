pretty_roc_plot = function(rmap_answers){
  numerical_summaries = rmap_answers$numerical_summaries
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
  
  roc_plot = rmap_answers$plot$roc_plot +
    xlab("1 - Specificity") + ylab("Sensitivity") +
    labs(title = "ROC Plot",
         subtitle = concordance_annotation)
  roc_plot
}
