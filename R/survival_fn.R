#' Calculate the survival function from the package survival
#'
#' Using survival_fit gotten by calling Surv in the package survival
#'
#' @param t A vector of times that you want to calculate the survival function of.
#' @param survival_fit An object of class \code{Surv}, the result of calling
#' \code{Surv} from the package \code{survival}.
#'
#' @return A vector of probabilities, one number for each time in \code{t}
#' equal to the survival function defined by \code{survival_fit} at that time.
#'
#' @examples
#' set.seed(1)
#' NNN = 100
#' N_bootstrap_reps = 10
#' cutoffs = c(0, 0.40, 1)
#' weighted_example = weighted_example_fn(NNN)
#' cohort_sampling_probability_dictionary = weighted_example$cohort_sampling_probability_dictionary
#' cohort_sample = weighted_example$cohort_sample
#' target_sample = weighted_example$target_sample
#' tStar = weighted_example$t_star
#' which_model = "r_B"
#' cohort_category = cohort_sample$category
#' target_category = target_sample$category
#' r = cohort_sample[[which_model]]
#' e = cohort_sample$eee
#' t = cohort_sample$ttt
#' design = list(targetCategory = target_category, c = cohort_category)
#' riskGroup = list(cutoffs = cutoffs)
#' rSummary = "mean"
#' bootstrap = N_bootstrap_reps
#' baseArgs = baseArgsFn(e, t, r, tStar, design, riskGroup, rSummary, bootstrap)
#' eee = baseArgs$e
#' ttt = baseArgs$t
#' risk = baseArgs$r
#' weight = baseArgs$weight
#' t_star = baseArgs$tStar
#' censoring_event = ifelse(eee == 0, 1, 0)
#' survival_object = Surv(time = ttt, event = censoring_event)
#' survival_fit = survfit(survival_object ~ 1, weights = weight)
#' n_1_uni = which(eee == 1 & ttt <= t_star)
#' n_2_uni = 1:length(eee)
#' survival_fn(ttt[n_1_uni], survival_fit)
#' aaa = 1/survival_fn(ttt[n_1_uni], survival_fit) * weight[n_1_uni]
#' survival_fn(t_star, survival_fit)
#'
#' @export
survival_fn = function(t, survival_fit){
  sss = survival_fit$surv
  tau = survival_fit$time
  sapply(t, function(this_t){
    sss[max(which(tau <= this_t))]
  })
}
