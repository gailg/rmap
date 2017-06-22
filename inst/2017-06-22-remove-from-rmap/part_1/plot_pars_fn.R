plot_pars_fn = function(
  plot_pars, grouping_name, hosmer_lemeshow, residuals){
                                                                       # risk_at
  plot_pars$risk_at = if(is.null(plot_pars$risk_at)){
    seq(0, 1, by = 0.05)
    } else plot_pars$risk_at
                                                                       # risk_labels
  plot_pars$risk_labels = if(is.null(plot_pars$risk_labels)){
    plot_pars$risk_at * 100
    } else plot_pars$risk_labels
                                                                       # x_max  
  plot_pars$x_max = if(is.null(plot_pars$x_max)){
    ceiling(max(hosmer_lemeshow$r) * 100) / 100
    } else plot_pars$x_max
                                                                       # x_at  
  plot_pars$x_at = if(is.null(plot_pars$x_at)){
    plot_pars$risk_at
    } else plot_pars$x_at
                                                                       # x_labels  
  plot_pars$x_labels = if(is.null(plot_pars$x_labels)){
    plot_pars$risk_labels
    } else plot_pars$x_labels
                                                                       # xlab  
  plot_pars$xlab = if(is.null(plot_pars$xlab)){
    grouping_name
    } else plot_pars$xlab
                                                                       # x_subheading  
  plot_pars$x_subheading = if(is.null(plot_pars$x_subheading)){
    0.005
    } else plot_pars$x_subheading
                                                                       # y_max_ad
  plot_pars$y_max_ad = if(is.null(plot_pars$y_max_ad)){
    ceiling(max(hosmer_lemeshow$upper) * 100)/100
    } else plot_pars$y_max_ad
                                                                       # y_at_ad
  plot_pars$y_at_ad = if(is.null(plot_pars$y_at_ad)){
    plot_pars$risk_at
    } else plot_pars$y_at_ad
                                                                       # y_labels_ad  
  plot_pars$y_labels_ad = if(is.null(plot_pars$y_labels_ad)){
    plot_pars$risk_labels
    } else plot_pars$y_labels_ad
                                                                       # y_lab_ad  
  plot_pars$ylab_ad = if(is.null(plot_pars$ylab_ad)){
    "Observed risk (%)"
    } else plot_pars$ylab_ad
                                                                       # y_subheading_ad  
  plot_pars$y_subheading_ad = if(is.null(plot_pars$y_subheading_ad)){
    plot_pars$y_max_ad - 0.02
    } else plot_pars$y_subheading_ad
                                                                       # y_max
  plot_pars[["y_max"]] = if(is.null(plot_pars[["y_max"]])){
    1 + ceiling(max(abs(unlist(lapply(residuals, function(this){
      this$delta$delta
      })))))
    } else plot_pars[["y_max"]]
                                                                       # y_at
  plot_pars[["y_at"]] = if(is.null(plot_pars[["y_at"]])){
    1:plot_pars[["y_max"]]
    } else plot_pars[["y_at"]]
                                                                       # y_labels
  plot_pars[["y_labels"]] = if(is.null(plot_pars[["y_labels"]])){
    plot_pars$y_at
    } else plot_pars[["y_labels"]] 
                                                                       # y_lab
  plot_pars[["ylab"]] = if(is.null(plot_pars[["ylab"]])){
    "Standardized residuals"
    } else plot_pars[["ylab"]] 
                                                                       # y_subheading
  plot_pars[["y_subheading"]] = if(is.null(plot_pars[["y_subheading"]])){
    plot_pars$y_max * 0.95
    } else plot_pars[["y_subheading"]]
                                                                       # first_column
  plot_pars$first_column = if(is.null(plot_pars$first_column)){
    TRUE
  } else plot_pars$first_column
                                                                       # xaxt_always  
  plot_pars$xaxt_always = if(is.null(plot_pars$xaxt_always)){
    FALSE
  } else plot_pars$xaxt_always
                                                                       # yaxt_always
  plot_pars$yaxt_always = if(is.null(plot_pars$yaxt_always)){
    FALSE
  } else plot_pars$yaxt_always
                                                                       # main
  plot_pars$main = if(is.null(plot_pars$main)){
    ""
  } else plot_pars$main
  
  plot_pars
  
}
