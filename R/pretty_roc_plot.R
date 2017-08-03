#' @title An annotated ROC plot using \code{rmap_answers}
#' 
#' @param rmap_answers Output from \code{rmap} or the
#' \code{rmap_answers} object from \code{rmap_random_sample}
#' or \code{rmap_weighted_sample}.
#' 
#' @param title A character string that specifies the title 
#' of the ROC plot. Defaults to \code{"ROC Plot"}.
#' 
#' @return A graphic that draws the ROC plot
#' together with a \code{title} and a subtitle.
#' The subtitle contains the concordance estimate
#' and confidence interval.
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
#' title = "This is to show off the title of the ROC plot"
#' pretty_roc_plot(rmap_answers, title)
#' 
#' @export
pretty_roc_plot = function(rmap_answers, title = "ROC Plot"){
  numerical_summaries = rmap_answers$numerical_summaries
  concordance_annotation_just_estimate = 
    paste0("Concordance = ",
           round(numerical_summaries$concordance["estimate"] * 100, 1),
           "%")
  concordance_annotation = if("lower" %in% names(numerical_summaries$concordance)){
    paste0(concordance_annotation_just_estimate,
           ", CI = (",
           round(numerical_summaries$concordance["lower"] * 100, 1),
           "%, ",
           round(numerical_summaries$concordance["upper"] * 100, 1), 
           "%)")
  } else {
    concordance_annotation_just_estimate
  }
  
  roc_plot = rmap_answers$plot$roc_plot +
    xlab("1 - Specificity") + ylab("Sensitivity") +
    labs(title = title,
         subtitle = concordance_annotation)
  roc_plot
}
