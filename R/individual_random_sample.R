#' @title \code{rmap_individual} on a random sample using quantile risk groups
#' 
#' @description A wrapper function that gives an easier call to
#' \code{rmap_individual} 
#' but without as many options, perhaps useful if your sample
#' is a random sample. Provided here to give you an example of
#' \code{rmap_individual}
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
#' @param N_bootstraps A non-negative integer
#' describing the number of bootstraps.  
#' Turn off bootstrapping with \code{N_bootstraps = 0}.
#' 
#' @return The same output as \code{rmap_individual}.
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
#' individual = individual_random_sample(e, t, r, t_star, N_bootstraps)
#' pretty_individual_risk_plot(individual)
#'              
#' @export
individual_random_sample = function(e, t, r, t_star, N_bootstraps){
  design = "random_sample"
  epsilon = length(e)^(-1/3)
  confidence_level = 0.95
  individual = rmap_individual(e, t, r, t_star, design, epsilon, 
                               N_bootstraps, confidence_level)
  individual
}