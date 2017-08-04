#' An annotated indivualized attribute diagram plot using results from 
#' \code{rmap_individual}
#' 
#' @param individual Output from \code{rmap_individual}.
#' 
#' @param title A character string that specifies the title 
#' of the individualized attribute diagram plot. 
#' Defaults to \code{"Individualized Attribute Diagram"}.
#' 
#' @param subtitle A character string that specifies the
#' subtitle.  Defaults to \code{""}.
#' 
#' @return A graphic that draws the attribute diagram
#' together with a \code{title} and a \code{subtitle}.

#' 
#' @examples 
#' data(random_sample_example)
#' xxx = random_sample_example
#' e = xxx$e
#' t = xxx$t
#' r = xxx$r
#' t_star = 10
#' design = "random_sample"
#' epsilon = length(e)^(-1/3)
#' N_bootstraps = 100
#' set.seed(1)
#' individual = rmap_individual(e, t, r, t_star, design, epsilon, N_bootstraps)
#' title = "This is to show off the title of the individualized attribute diagram"
#' subtitle = "I have nothing more to say"
#' pretty_individual_risk_plot(individual, title, subtitle)
#' 
#' @import ggplot2
#' @export
pretty_individual_risk_plot = function(individual, 
  title = "Individualized Attribute Diagram",
  subtitle = " ")
{
  individual$risk_plot + labs(title = title, subtitle = subtitle)
}
