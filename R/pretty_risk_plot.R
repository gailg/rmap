#' An annotated attribute diagram plot using \code{rmap_answers}
#' 
#' @param rmap_answers Output from \code{rmap} or the
#' \code{rmap_answers} object from \code{rmap_random_sample}
#' or \code{rmap_weighted_sample}.
#' 
#' @param title A character string that specifies the title 
#' of the attribute diagram plot. Defaults to 
#' \code{"Attribute Diagram"}.
#' 
#' @return A graphic that draws the attribute diagram
#' together with a \code{title} and a subtitle.
#' The subtitle contains the goodness of fit test statistic
#' and its p-value.
#' 
#' @examples 
#' data(random_sample_example)
#' xxx = random_sample_example
#' e = xxx$e
#' t = xxx$t
#' r = xxx$r
#' t_star = 10
#' K = 4
#' N_bootstraps = 100
#' set.seed(1)
#' the_random_sample = rmap_random_sample(e, t, r, t_star, K, N_bootstraps)
#' rmap_answers = the_random_sample$rmap_answers
#' title = "This is to show off the title of the attribute diagram plot"
#' pretty_risk_plot(rmap_answers, title)
#' 
#' @import ggplot2
#' @export


pretty_risk_plot = function(rmap_answers, title = "Attribute Diagram"){
  numerical_summaries = rmap_answers$numerical_summaries
  gof = if( !is.null(numerical_summaries$gof_asymptotic) ){
    numerical_summaries$gof_asymptotic
  } else {
    numerical_summaries$gof_bootstrap
  }
  risk_annotation = paste0("GOF = ", 
                           round(gof["statistic"], 4),
                           ", P-Value = ", 
                           round(gof["p_value"], 4))
  rmap_answers$plots$risk_plot +
    labs(title = title,
         subtitle = risk_annotation)
  
}
