risk_quantile_boxplots = function(list_of_risk_models, K, risk_max = 1, text_x = NULL, text_y = NULL){
  text_x = 100 * if(is.null(text_x)) risk_max/2 else text_x
  text_y = if(is.null(text_y)) K else text_y
  risk_model_names = names(list_of_risk_models)
  N_risk_models = length(risk_model_names)
  par(mfrow = c(N_risk_models, 1), mar = c(0, 0, 0, 0), oma = c(5, 4, 5, 4))
  xaxt_uni = c(rep("n", N_risk_models - 1), "s")
  mapply(function(name, xaxt){
    rrr = list_of_risk_models[[name]] * 100
    r_quantiles = grouping_quantile_fn(rrr, K)
    r_split = split(rrr, as.factor(r_quantiles))
    boxplot(rev(r_split), 
            horizontal = TRUE, 
            col = "gray",
            xaxt = xaxt, 
            las = 1,
            ylim = c(0, risk_max * 100))
    text(x = text_x, y = text_y, labels = name, pos = 3)
    }, risk_model_names, xaxt_uni)
  mtext(
    "Assigned risk (%)", at = c(.5), side = 1, line = 3, outer = TRUE)
} 
