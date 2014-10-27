test_statistics_p_values_one_risk_model_fn = function(risk_model, params){

  data = risk_model$data
  
  overall = c(
    hosmer_lemeshow = unname(hosmer_lemeshow_fn(
      data,
      rep(1, nrow(data)))$ChiSq["HL_pval"]),
    combined_outcome_mortality_fn(
      data, 
      regression_example_1_fn, 
      output = list("T", "p")))
  
  weighted = combined_outcome_mortality_fn(
    data, 
    regression_example_2_fn, 
    output = list("T", "p"), rrr = data$r)
  
  groupings_to_include = if(is.null(params$groupings_to_include)){
    names(risk_model$grouping)
  } else {
    params$groupings_to_include
  }
  
  groupings = nlapply(groupings_to_include, function(grouping_name){
    c(
      hosmer_lemeshow = unname(hosmer_lemeshow_fn(
        data,
        risk_model$groupings[[grouping_name]]$grouping)$ChiSq["HosmerLemeshow"]),
      combined_outcome_mortality_fn(
        data, 
        regression_example_3_fn, 
        output = list("T", "p"),
        inside_each_grouping = risk_model$groupings[[grouping_name]]))
  })
  
  answer = unlist(c(
    overall = overall, 
    weighted = weighted, 
    groupings))
  names(answer) = gsub("\\.", "_", names(answer))
  answer
}
