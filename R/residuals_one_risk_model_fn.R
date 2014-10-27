residuals_one_risk_model_fn = function(risk_model, params){
  data = risk_model$data
  
  groupings_to_include = if(is.null(params$groupings_to_include)){
    names(risk_model$grouping)
  } else {
    params$groupings_to_include
  }
  nlapply(groupings_to_include, function(grouping_name){
    hosmer_lemeshow = {
      inside_each_grouping = risk_model$groupings[[grouping_name]] 
      grouping = inside_each_grouping$grouping
      hosmer_lemeshow_fn(data, as.integer(grouping))
    }
    residuals = combined_outcome_mortality_fn(
      data, regression_example_3_fn, output = list(),
      inside_each_grouping = risk_model$groupings[[grouping_name]])
    list(hosmer_lemeshow = hosmer_lemeshow,
         residuals = residuals)
  })
}
