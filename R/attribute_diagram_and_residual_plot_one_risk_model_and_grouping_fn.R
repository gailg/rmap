attribute_diagram_and_residual_plot_one_risk_model_and_grouping_fn = function(
  residuals_one_risk_model, grouping_name, plot_pars){
  risk_model_and_grouping = residuals_one_risk_model[[grouping_name]]
  hosmer_lemeshow = risk_model_and_grouping$hosmer_lemeshow$piHatSummary
  residuals = risk_model_and_grouping$residuals
  plot_pars = plot_pars_fn(plot_pars, grouping_name, hosmer_lemeshow, residuals)

          
  with(hosmer_lemeshow, with(plot_pars,
    plot(r, piHat, type = "p", pch = 16, cex = 1.5,
         xlim = c(0, x_max), ylim = c(0, y_max_ad),
         xlab = "", ylab = "",
         xaxt = "n", yaxt = "n")))
  lapply(1:nrow(hosmer_lemeshow), function(jjj){
    x = rep(hosmer_lemeshow[jjj, "r"], 2)
    y = unlist(hosmer_lemeshow[jjj, c("lower", "upper")])
    lines(x, y)
    points(x, y, pch = "_", cex = 1)
    })
  abline(a = 0, b = 1, col = "red", lty = 1)
  with(plot_pars, text(x = x_subheading, 
                       y = y_subheading_ad, 
                       "Attribute diagram", pos = 4))
  with(plot_pars, if(xaxt_always){
    axis(1, at = x_at, labels = x_labels)
  })
  with(plot_pars, if(first_column | yaxt_always){
    axis(2, at = y_at_ad, labels = y_labels_ad)
    })
  
  event_name_uni = names(residuals)
  last_event_name = rev(event_name_uni)[1]
  yaxt = with(plot_pars, if(first_column | yaxt_always){
    "s"
  } else {
    "n"
  })

  lapply(event_name_uni, function(event_name){
    event = residuals[[event_name]]
    delta = event$delta
    delta$x = hosmer_lemeshow$r 
    xaxt = "n"
    with(plot_pars, {
      plot(delta$x, delta$delta, 
           main = "",
           xlim = c(0, x_max),
           ylim = c(-y_max, y_max),
           xlab = "",
           ylab = "",
           xaxt = xaxt,
           yaxt = yaxt, 
           pch = 16, cex = 1.5)
      })
    abline(h = c(-2, 0, 2), col = "red", lty = c(2, 1, 2))
    with(plot_pars, text(x =  x_subheading, 
         y = y_subheading, 
         event_name, 
         pos = 4))
    if(event_name == last_event_name) with(plot_pars,
      axis(1, at = x_at, labels = x_labels))
    })
  
  plot_pars
  }
