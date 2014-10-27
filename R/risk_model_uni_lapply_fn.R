risk_model_uni_lapply_fn = function(risk_model_uni, one_risk_model_fn, params){
  nlapply(names(risk_model_uni), function(name){
    risk_model = risk_model_uni[[name]]
    one_risk_model_fn(risk_model, params)
      })
}
