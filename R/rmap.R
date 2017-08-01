#' @title \code{rmap} is the main function of the package
#' 
#' @description For a risk model, produce the necessary objects 
#' to estimate and provide a confidence interval for concordance, 
#' plot an ROC plot, perform a grouped goodness-of-fit test, 
#' and plot a grouped attribute diagram.
#' 
#' @param e Throughout this help page, let \code{N} be the number 
#' of people in your data set.  \code{e} is a vector of length 
#' \code{N}, the \code{n}-th element
#' containing the event of the \code{n}-th person.
#' The event for one subject can either be a 
#' \code{0} (censored), \code{1} (disease) or \code{2} 
#' (death from other causes).
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
#' @param r A vector of length \code{N}, 
#' the \code{n}-th element containing
#' the risk assigned to the \code{n}-th person 
#' by the personal predictive model. 
#' This is the probability predicted by the model,
#' of the outcome occuring before the
#' competing risk and before \code{t_star}. 
#' The goal of the \code{rmap} package is to assess the validity and
#' calibration of this model. Acceptable values are real numbers 
#' between \code{0} and \code{1}.
#' @param tStar A positive number equal to the duration of the the study.
#' The risk model that you are evaluating expresses the risk of the outcome
#' occuring within the time period \code{[0, t_star]} and before the competing
#' risk.
#' @param design One of the following choices
#' \itemize{
#' \item{\code{"random_sample"}: }{Using \code{design = "random_sample"}
#'   signals to \code{rmap} that you obtained your data using a
#'   random sample.
#' }
#' \item{\code{list(category = category, N_two_stage = N_two_stage)}: }{If your
#'   design is a two-stage sample, name your two-stage sampling
#'   categories with capital letters beginning with \code{"A"},
#'   \code{"B"}, etc., let \code{category} be the 
#'   vector of length \code{N}, the
#'   and \code{n}-th  \code{N_two_stage} be a named vector
#'   counting the number
#'   of subjects from the first stage that fell into each category.
#'   (For example, if the two categories were \code{"A"} and \code{"B"}, 
#'   and there were \code{13} in the first stage who were categorized
#'   to \code{"A"} and \code{42} to \code{"B"}, then define
#'   \code{N_two_stage = c(A = 13, B = 42)}; you would of course have
#'   much larger numbers than \code{13} and \code{42}).
#'   Specify \code{design = list(category = category, N_two_stage = N_two_stage)}.
#' }
#' \item{\code{list(category = category, target_category = target_category)}: }{ If your
#'   cohort sample does not match your target population and you have 
#'   a sample from your target population and both samples contain 
#'   relevant categories, label the categories beginning with \code{"A"},
#'   \code{"B"}, etc, and let \code{category} be the vector of cohort subjects'
#'   categories and \code{target_category} be the vector of target
#'   subjects' categories, and specify 
#'   \code{design = list(category = category, target_category = target_category)}.
#' }
#' }
#' @param riskGroup Describes the way you want the subjects to be divided
#' into risk groups. Choose one of the following named lists:
#' \itemize{
#' \item{\code{list(K = K)}: }{Define \code{K} to be a positive integer
#' and let \code{rmap} divide your sample into \code{K} quantiles.  For 
#' example, if \code{K = 4}, your risk groups will consist of \code{4}
#' quartiles, the first group, \code{k = 1}, containing the subjects
#' with the smallest risks \code{r}, the fourth group, \code{k = 4}, 
#' containing the subjects with the largest risks.
#' }
#' \item{\code{list(cutoffs = cutoffs)}: }{Risk groups can also be 
#' assigned to each subject in the dataset based on risk "cutoffs". 
#' If you wished to create three risk groups, say all subjects having 
#' an assigned risk between 0 and 0.2 to be in risk group 1, 
#' all subjects with assigned risk between 0.2 and 0.5 to be in risk 
#' group 2, and all subjects with assigned risk between 0.5 and 1 
#' to be in risk group 3, define \code{cutoffs = c(0, 0.2, 0.5, 1)}.
#' }
#' \item{\code{list(K = k)}: }{You can manually assign risk groups using 
#' an integer vector \code{k} containing risk group assignments for each 
#' subject in the dataset.  Define \code{K} to be the total number of 
#' risk groups.  Then your vector \code{k} is a risk group assignment  
#' in \code{\{1, ..., K\}} for each subject in the dataset.  
#' Subjects with risk group assignment \code{= 1} should have the smallest 
#' assigned risks (\code{r}), and subjects with the risk group assignment 
#' \code{= K} should have the highest assigned risks.
#' }
#' }
#' @param r_summary A summary statistic for summarizing the 
#' assigned risks for all subjects in a risk group.  
#' There are four options in specifying the \code{r_summary}: 
#' \itemize{
#' \item{\code{"mean"}: } {Summarize each risk group with
#' code{mean(r)} over all values of \code{r} in that risk group.
#' }
#' \item{\code{"median"}: } {Summarize each risk group with
#' code{median(r)} over all values of \code{r} in that risk group.
#' }
#' \item{\code{"midpoint"}: } {If 
#' \code{risk_group = list(cutoffs = c(0, 0.2, 0.5, 1))}, then the 
#' \code{r_summary} would be assigned \code{c(0.1, 0.35, 0.75)}.
#' }
#' \item{A user supplied summary vector } {You may manually specify
#' your own values for \code{r_summary}.
#' To use this option, define \code{r_summary} to be a 
#' numeric vector of length \code{K} specifying your summary statistic
#' for each risk group.
#' }
#' }
#' @param N_bootstraps A non-negative integer
#' describing the number of bootstraps.  
#' Turn off bootstrapping with code{N_bootstraps = 0}.
#' @param confidence_level A positive number less than \code{1}, 
#' defaulted to \code{0.95}, specifying the confidence levels of the
#' confidence intervals inside \code{rmap}
#' @return A list containing the following two objects
#' \itemize{
#' \item{\code{numerical_summaries}: }{A list
#' }
#' \item{\code{plots}: }{A list
#' }
#' }
#' @examples
#' #--------------------------------------------------- random sample
#' data(random_sample_example)
#' xxx = random_sample_example
#' head(xxx)
#' e = xxx$e
#' t = xxx$t
#' r = xxx$r
#' t_star = 10
#' design = "random_sample"
#' risk_group = list(K = 4)
#' r_summary = "mean"
#' N_bootstraps = 100
#' confidence_level = 0.95
#' set.seed(1)
#' rmap_answers = rmap(e, t, r, t_star, design, risk_group, r_summary, 
#'                     N_bootstraps, confidence_level)
#' names(rmap_answers)
#' rmap_answers$numerical_summaries
#' head(rmap_answers$plots$df_for_roc_plot)
#' rmap_answers$plots$roc_plot
#' pretty_roc_plot_fn(rmap_answers)
#' rmap_answers$plots$risk_plot
#' pretty_risk_plot_fn(rmap_answers)
#' #------------------------------------------------- two-stage sample
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
#' risk_group = list(K = 3)
#' r_summary = "mean"
#' N_bootstraps = 100
#' confidence_level = 0.95
#' set.seed(3)
#' rmap_answers = rmap(e, t, r, t_star, design, risk_group, r_summary, 
#'                     N_bootstraps, confidence_level)
#' #---------------------------------------------------- weighted sample                   
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
#' risk_group = list(cutoffs = cutoffs)
#' r_summary = "mean"
#' N_bootstraps = 100
#' confidence_level = 0.95
#' set.seed(5)
#' rmap_answers = rmap(e, t, r, t_star, design, risk_group, r_summary, 
#'                     N_bootstraps, confidence_level)                    
#' @export                     

rmap = function(e, t, r, t_star, design, risk_group, r_summary, N_bootstraps, 
                confidence_level = 0.95, N_cores = 1, verbose = FALSE){
  baseArgs = base_args_fn(
    e, t, r, t_star, design, risk_group, r_summary, N_bootstraps, 
    confidence_level, N_cores, verbose)
  rmap_fn(baseArgs)
}