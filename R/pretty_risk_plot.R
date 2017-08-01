pretty_risk_plot = function(rmap_answers){
  numerical_summaries = rmap_answers$numerical_summaries
  gof = if( !is.null(numerical_summaries$gof_asymptotic) ){
    numerical_summaries$gof_asymptotic
  } else {
    numerical_summaries$gof_bootstrap
  }
  risk_annotation = paste0("GOF = ", 
                           round(gof["statistic"], 4),
                           ", P-Value = ", 
                           round(gof["p_value"], 4))
  rmap_answers$plots$risk_plot +
    labs(title = "Attribute Diagram",
         subtitle = risk_annotation)
  
}
