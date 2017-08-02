pretty_individual_risk_plot = function(individual, subtitle = " "){
  individual$risk_plot +
    labs(title = "Individualized Attribute Diagram",
         subtitle = subtitle)
}
