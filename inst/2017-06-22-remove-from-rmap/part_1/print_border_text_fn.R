print_border_text_fn = function(plot_pars, risk_model_name_uni){
  with(plot_pars, 
       mtext(xlab, at = 0.5, side = 1, line = 3, outer = TRUE))
  with(plot_pars,
       mtext(c("Standardized residual", ylab_ad),
             at = 1 - 0.5 * c(1 + N_rows, 1)/N_rows,
             side = 2, 
             line = 3,
             outer = TRUE))
  with(plot_pars, 
       mtext(risk_model_name_uni, 
             at = (seq(1,N_columns) - 0.5) / N_columns,
             side = 3,
             line = 1, 
             outer = TRUE))
  with(plot_pars, 
       mtext(main,
             at = 0.5,
             side = 3,
             line = 4,
             outer = TRUE))
}
