attribute_diagram_and_residual_plot_fn = function(
  residuals_risk_model_uni, grouping_name, plot_pars = NULL){
  N_rows = 1 + length(residuals_risk_model_uni[[1]][[1]][["residuals"]])
  N_columns = length(residuals_risk_model_uni)
  risk_model_name_uni = names(residuals_risk_model_uni)
  margin = if(is.null(plot_pars)) 4 else 0
  par(mfcol = c(N_rows, N_columns), 
      mar = c(margin, margin, 0, 0), 
      oma = c(5, 4, 5, 4))
  new_plot_pars_uni = lapply(risk_model_name_uni, function(risk_model_name){
    first_column = TRUE
    plot_pars = if(is.null(plot_pars)){
      list(first_column = first_column,
           xaxt_always = TRUE,
           yaxt_always = TRUE)
      } else {
        first_column = if(risk_model_name == risk_model_name_uni[1]){
          TRUE
          } else {
            FALSE
            }
        c(plot_pars, list(first_column = first_column))
        }
    residuals_one_risk_model = residuals_risk_model_uni[[risk_model_name]]
    attribute_diagram_and_residual_plot_one_risk_model_and_grouping_fn(
      residuals_one_risk_model, grouping_name, plot_pars)
    })
  plot_pars = new_plot_pars_uni[[1]]
  plot_pars$N_rows = N_rows
  plot_pars$N_columns = N_columns
  print_border_text_fn(plot_pars, risk_model_name_uni)
  
  
  }
