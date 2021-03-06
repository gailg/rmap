#' @title Individualized attribute diagram
#' 
#' @description Provide the plot and the data for an attribute
#' diagram using an epsilon kernel nearest neighbor estimate of 
#' outcome probabilty for each distinct value of assigned risk
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
#' @param design One of the following choices
#' \itemize{
#' \item{\code{"random_sample"}: }{Using \code{design = "random_sample"}
#'   signals to \code{rmap} that you obtained your data using a
#'   random sample.
#' }
#' \item{\code{list(category = category, N_two_stage = N_two_stage)}: }{If your
#'   design is a two-stage sample, name your two-stage sampling
#'   categories with capital letters beginning with \code{"A"},
#'   \code{"B"}, etc. Let \code{category} be the 
#'   vector of length \code{N} with
#'   \code{n}-th element containing the category of the \code{n}-th
#'   person.  Let \code{N_two_stage} be a named vector
#'   counting the number
#'   of subjects from the first stage that fell into each category.
#'   (For example, if the two categories were \code{"A"} and \code{"B"}, 
#'   and there were \code{13} in the first stage who were categorized
#'   \code{"A"} and \code{42} categorized \code{"B"}, then define
#'   \code{N_two_stage = c(A = 13, B = 42)}; you would of course have
#'   much larger numbers than \code{13} and \code{42}).
#'   Specify \code{design = list(category = category, N_two_stage = N_two_stage)}.
#' }
#' \item{\code{list(category = category, target_category = target_category)}: }{ If 
#' covariates of the participants of the cohort study differ from those of the 
#' population for which your predictive model is targeted, you can weight the 
#' cohort particpants using an additional sample, a target sample, whose 
#' covariate categories match the target population.  Classify subjects in both
#' cohort sample and target sample into joint covariate categories, labeled
#' with capital letters beginning with \code{"A"}, \code{"B"}, etc.  Let 
#' \code{category} be the vector of length \code{N} with \code{n}-th element
#' containing the covariate category of the \code{n}-th person.  Let
#' \code{target_category} be a vector containing one element for each
#' person in the target sample recording his/her covariate category.
#' Specify 
#' \code{design = list(category = category, target_category = target_category)}.
#' }
#' \item{\code{list(category = category, N_target = N_target)}: } {
#' Continuing with the previous choice for \code{design}, if instead of providing
#' \code{rmap} with the vector \code{target_cateogry} you wish to provide rmap
#' with counts of the numbers of people in the target population that fell 
#' in each covariate category, let \code{N_target} be a named vector counting 
#' the number of subjects from the target sample that fell into each category.
#' (For example, if you had two categories labeled \code{"A"} and \code{"B"}, 
#' and there were \code{13} in the target sample who fell into covariate
#' category \code{"A"} and \code{42} into \code{"B"}, then define
#' \code{N_target = c(A = 13, B = 42)}.)
#' Specify \code{design = list(category = category, N_target = N_target)}. 
#' }
#' }
#' @param epsilon
#' A small positive number specifying
#' the  kernel neighborhoods used to calculate observed risk
#' at each distinct assigned risk. Asymptotic theory suggests
#' good behavior for \code{epsilon = N^(-1/3)}.
#' 
#' @param N_bootstraps A non-negative integer
#' describing the number of bootstraps.  
#' Turn off bootstrapping with \code{N_bootstraps = 0}.
#' 
#' @param confidence_level A positive number less than \code{1}, 
#' defaulted to \code{0.95}, specifying the confidence levels of the
#' confidence intervals inside \code{rmap}
#' 
#' @return A list containing
#' \itemize{
#' \item{\code{df_for_risk_plot}: }{A data.frame containing the columns 
#' \code{assigned_risk} and \code{observed_risk}. These are the points 
#' that are plotted in the indidualized attribute diagram.
#' }
#' \item{\code{risk_plot}: }{The individualized attribute diagram.
#' }
#' }
#' 
#' @examples 
#' #------------------------------------------------------------ random sample
#' data(random_sample_example)
#' xxx = random_sample_example
#' head(xxx)
#' e = xxx$e
#' t = xxx$t
#' r = xxx$r
#' t_star = 10
#' design = "random_sample"
#' epsilon = length(e)^(-1/3)
#' N_bootstraps = 100
#' set.seed(1)
#' individual = rmap_individual(e, t, r, t_star, design, epsilon, N_bootstraps)
#' pretty_individual_risk_plot(individual, "random sample")
#' #---------------------------------------------------------- two-stage sample
#' data(two_stage_sample_example)
#' xxx = two_stage_sample_example
#' head(xxx)
#' e = xxx$e
#' t = xxx$t
#' r = xxx$r
#' t_star = 10
#' N_first_stage = c(A = 132, B = 168)
#' N_first_stage
#' category = xxx$category
#' design = list(N_first_stage = N_first_stage, category = category)
#' epsilon = length(e)^(-1/3)
#' N_bootstraps = 100
#' set.seed(2)
#' individual = rmap_individual(e, t, r, t_star, design, epsilon, N_bootstraps)
#' pretty_individual_risk_plot(individual, "two-stage sample")
#' #----------------------------------------------------------- weighted sample 
#' data(weighted_example_cohort_sample)
#' xxx = weighted_example_cohort_sample
#' head(xxx)
#' e = xxx$e
#' t = xxx$t
#' r = xxx$r
#' t_star = 10
#' data(weighted_example_target_sample)
#' head(weighted_example_target_sample)
#' target_category = weighted_example_target_sample$category
#' category = xxx$category
#' design = list(target_category = target_category, category = category)
#' cutoffs = c(0, 0.20, 1)
#' epsilon = length(e)^(-1/3)
#' N_bootstraps = 100
#' set.seed(3)
#' individual = rmap_individual(e, t, r, t_star, design, epsilon, N_bootstraps)
#' pretty_individual_risk_plot(individual, "weighted sample")
#' 
#' 
#' @export
#' 
rmap_individual = function(e, t, r, t_star, design, epsilon, N_bootstraps, confidence_level = 0.95){
  risk_group = list(epsilon = epsilon)
  r_summary = "mean"
  N_cores = 0
  verbose = FALSE
  baseArgs = base_args_fn(
    e, t, r, t_star, design, risk_group, r_summary, N_bootstraps, 
    confidence_level, N_cores, verbose)
  rmap_individual_fn(baseArgs)
}
