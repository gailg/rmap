standardized_residuals = function(e, t, ...){
  risk_model_uni = risk_model_uni_fn(e, t, ...)
  params = list(groupings_to_include = NULL)
  answer = risk_model_uni_lapply_fn(
    risk_model_uni, 
    residuals_one_risk_model_fn, 
    params)
  class(answer) = "standardized_residuals"
  answer
  }
