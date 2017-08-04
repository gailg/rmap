#' rmap.
#' 
#' @name rmap
#' @docType package
#' @import CompQuadForm survival ggplot2 gridExtra grid
NULL
#' 
#' A random sample example
#' 
#' A dataset gotten using a random sample design for trying out \code{rmap}.
#'  The variables are
#' 
#' \itemize{
#'  \item e. The event
#'  \item t. The time of the event.
#'  \item r. The assigned risk.
#' }
#' 
#' @docType data
#' @keywords rmap_datasets
#' @name random_sample_example
#' @usage data(random_sample_example)
#' @format A data.frame with 300 rows and 3 variables
NULL
#' 
#' A two-stage sample example
#' 
#' A dataset gotten using a two-stage sample design for trying out \code{rmap}.
#' This is the second stage sample.  To obtain the counts from the first stage
#' use \code{N_first_stage}
#'  The variables are
#' 
#' \itemize{
#'  \item e. The event
#'  \item t. The time of the event.
#'  \item r. The assigned risk.
#'  \item category. The two-stage sample category. 
#' }
#' 
#' @docType data
#' @keywords rmap_datasets
#' @name two_stage_sample_example
#' @usage data(two_stage_sample_example)
#' @format A data.frame with 210 rows and 4 variables
NULL
#' 
#' The data from the first stage
#' 
#' A named vector that counts the number of people
#' in the first stage in each two-stage category.
#' The second stage data are in 
#'  \code{two_stage_sample_example}
#' 
#' @docType data
#' @keywords rmap_datasets
#' @name N_first_stage
#' @usage data(N_first_stage)
#' @format A named vector with two elements names \code{"A"} and \code{"B"}
NULL
#' The cohort sample of a weighted sample example
#' 
#' A dataset whose covariates do not match those of the target population
#' The related target sample is \code{weighted_example_target_sample}
#' The variables of the cohort sample are
#' 
#' \itemize{
#'  \item e. The event
#'  \item t. The time of the event.
#'  \item r. The assigned risk.
#'  \item category. The joint-covariate category to be matched with that of the target sample
#' }
#' 
#' @docType data
#' @keywords rmap_datasets
#' @name weighted_example_cohort_sample
#' @usage data(weighted_example_cohort_sample)
#' @format A data.frame with 224 rows and 4 variables
NULL
#' The target sample of a weighted sample example
#' 
#' A dataset sampled from  the target population
#' This provides the dataset \code{weighted_example_target_sample}
#' with joint-covariate category data used by \code{rmap} to obtain
#' weights to perform a weighted \code{rmap} analysis.  The other variables 
#' \code{x_1} and \code{x_2} were presumably used to create determine
#' \code{category}, but you may ignore them.
#' 
#' \itemize{
#'  \item x_1. A variable you may ignore
#'  \item x_2. Another variable you may ignore
#'  \item category. The joint-covariate category to be matched with that of the cohort sample
#' }
#' 
#' @docType data
#' @keywords rmap_datasets
#' @name weighted_example_target_sample
#' @usage data(weighted_example_target_sample)
#' @format A data.frame with 400 rows and 3 variables
NULL
#' A random sample with two risk models
#' 
#' A dataset gotten using a random sample design
#' containing two risk models for trying out \code{rmap}.
#'  The variables are
#' 
#' \itemize{
#'  \item e. The event
#'  \item t. The time of the event.
#'  \item r1. The risk assigned by model 1.
#'  \item r2. The risk assigned by model 2, a noisy version of model 1.
#' }
#' 
#' @docType data
#' @keywords rmap_datasets
#' @name two_model_comparison_example
#' @usage data(two_model_comparison_example)
#' @format A data.frame with 1000 rows and 4 variables
NULL

