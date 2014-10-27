score_statistics = function(e, t, ...){
  risk_model_uni = risk_model_uni_fn(e, t, ...)
  params = list(groupings_to_include = NULL)
  do.call(
    cbind, 
    risk_model_uni_lapply_fn(
      risk_model_uni, 
      test_statistics_p_values_one_risk_model_fn, 
      params))
  }
