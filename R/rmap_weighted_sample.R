#' @title \code{rmap} and \code{rmap_individual} on a weighted sample using cutoff risk groups
#' 
#' @description A wrapper function that gives an easier call to
#' \code{rmap} and \code{rmap_individual} 
#' but without as many options, perhaps useful if your sample
#' is a weighted sample. Provided here to give you an example of
#' a call to \code{rmap} and \code{rmap_individual}
#' 
#' @param e Throughout this help page, let \code{N} be the number 
#' of people in your data set.  \code{e} is a vector of length 
#' \code{N}, the \code{n}-th element
#' containing the event of the \code{n}-th person.
#' The event for one subject can either be a 
#' \code{0} (censored), \code{1} (disease), or \code{2} 
#' (death from other causes).
#' 
#' @param t A vector of length \code{N}, the 
#' \code{n}-th element containing the time 
#' until event for the \code{n}-th person. 
#' Mathematically speaking, let 
#' \code{t_1} be the time until outcome, 
#' \code{t_2} the time until competing risk,
#' and \code{t_0} the time until censoring. 
#' Then \code{t} is the minimum of
#' \code{t_1}, \code{t_2}, \code{t_0}, 
#' and \code{e} records which of the events occured first.
#' Acceptable values for 
#' \code{t} are positive real numbers.
#' 
#' @param r A vector of length \code{N}, 
#' the \code{n}-th element containing
#' the risk assigned to the \code{n}-th person 
#' by the personal predictive model. 
#' This is the probability, predicted by the risk model,
#' of the outcome occurring before the
#' competing risk and before \code{t_star}. 
#' The goal of the \code{rmap} package is to assess the validity and
#' calibration of this model. Acceptable values are real numbers 
#' between \code{0} and \code{1}.
#' 
#' @param t_star A positive number equal to the duration of the the study.
#' The risk model that you are evaluating expresses the risk of the outcome
#' occuring within the time period \code{[0, t_star]} and before the competing
#' risk.
#' 
#' @param cutoffs 
#' If you wished to create three risk groups, say all subjects having 
#' an assigned risk between 0 and 0.2 to be in risk group 1, 
#' all subjects with assigned risk between 0.2 and 0.5 to be in risk 
#' group 2, and all subjects with assigned risk between 0.5 and 1 
#' to be in risk group 3, define \code{cutoffs = c(0, 0.2, 0.5, 1)}.
#' 
#' @param N_bootstraps A non-negative integer
#' describing the number of bootstraps.  
#' Turn off bootstrapping with \code{N_bootstraps = 0}.
#' 
#' @return A list containing \code{rmap_answers}, which are outputs from 
#' \code{rmap}, and \code{individual}, which are outputs from
#' \code{rmap_individual}
#' 
#' @examples 
#' data(weighted_example_cohort_sample)
#' xxx = weighted_example_cohort_sample
#' head(xxx)
#' e = xxx$e
#' t = xxx$t
#' r = xxx$r
#' t_star = 10
#' data(weighted_example_target_sample)
#' target_sample = weighted_example_target_sample
#' head(target_sample)
#' target_category = target_sample$category
#' category = xxx$category
#' cutoffs = c(0, 0.20, 1)
#' N_bootstraps = 100
#' set.seed(1)
#' the_weighted_sample = rmap_weighted_sample(
#'   e, t, r, category, target_category, t_star, cutoffs, N_bootstraps)
#' rmap_answers = the_weighted_sample$rmap_answers
#' individual = the_weighted_sample$individual
#' the_message = paste("rmap_weighted_sample", sep = "\n")
#' grid.arrange(textGrob(the_message), 
#'              pretty_risk_plot(rmap_answers), 
#'              pretty_roc_plot(rmap_answers), 
#'              pretty_individual_risk_plot(individual),
#'              ncol = 2)
#'              
#' @export
rmap_weighted_sample = function (
  e, t, r, category, target_category, t_star, cutoffs, N_bootstraps){
  design = list(target_category = target_category, category = category)
  risk_group = list(cutoffs = cutoffs)
  r_summary = "mean"
  confidence_level = 0.95
  rmap_answers = rmap(e, t, r, t_star, design, risk_group, r_summary, 
                      N_bootstraps, confidence_level)
  epsilon = length(e)^(-1/3)
  individual = rmap_individual(e, t, r, t_star, design, epsilon,
                               N_bootstraps, confidence_level)
  
  list(rmap_answers = rmap_answers,
       individual = individual)
}
