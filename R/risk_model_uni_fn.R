risk_model_uni_fn = function(e, t, ...){
  e_outcome = as.numeric(e == 1)
  e_mortality = as.numeric(e == 2)
  e_combined = e_outcome + e_mortality
  risk_models = list(...)
  nlapply(names(risk_models), function(risk_model_name){
    risk_model = risk_models[[risk_model_name]]
    Lambda_outcome = risk_model$Lambda_outcome
    Lambda_mortality = if(is.null(risk_model$Lambda_mortality)){
      rep(0, length(e))
    } else {
      risk_model$Lambda_mortality
    }
    Lambda_mortality_den = if(is.null(risk_model$Lambda_mortality)){
      e_mortality
    } else {
      risk_model$Lambda_mortality
    }
    rrr = risk_model$r
    ddd = data.frame(
      e, t, e_outcome, e_mortality, e_combined,
      Lambda_outcome,
      Lambda_mortality,
      Lambda_mortality_den,
      Lambda_combined = Lambda_outcome + Lambda_mortality,
      Lambda_combined_den = Lambda_outcome + Lambda_mortality_den,
      r = rrr)
    groupings = nlapply(names(risk_model$groupings), function(grouping_name){
      user_grouping = risk_model$groupings[[grouping_name]]
      grouping_fn(rrr, user_grouping)
    })
    list(data = ddd, groupings = groupings)
  })
}
