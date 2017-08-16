#' Concordance or area under the receiver operating characteristic curve
#'
#' Concordance, or area under the receiver operating characteristic curve,
#' in the presence of competing risk, and allowing for two-stage sampling
#' and weights.
#' Also sensitivity and specificity are provided to draw the roc curve.
#'
#' @param baseArgs A list provided by \code{baseArgsFn}.  The objects
#' required by \code{concordance_fn} are \code{e}, \code{t}, \code{r},
#' \code{weight}, and \code{tStar}.
#'
#' @return A list containing
#' \itemize{
#'   \item{\code{concordance} }{
#'   A positive number equal to the concordance, the probability of
#'   a randomly chosen case has higher assigned risk than a randomly
#'   chosen control.  With the possibility of censoring and competing
#'   risks, the definition of cases and controls needs to be considered.
#'   }
#'   \item{\code{roc} }{
#'   A list containing
#'   \itemize{
#'     \item{\code{ccc}: }{
#'     A vector containing the unique and sorted \code{baseArgs$r}.
#'     }
#'     \item{\code{sensitivity}: }{
#'     A vector containing the sensitivity, at each point in \code{ccc}.
#'     (The sensitivity at \code{c} is the probability that a randomly chosen case
#'     exceeds \code{c}.)
#'     }
#'     \item{\code{specificity}: }{
#'     A vector containing the specificity at each point in \code{ccc}.
#'     (The specificty at \code{c} is the probability that a randmly chosen control
#'     does not exceed \code{c}.)  A plot of (\code{1 - specificity, sensitivity})
#'     is the roc curve.
#'     }
#'   }
#'   }
#' }
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
#' design = list(target_category = target_category, c = cohort_category)
#' riskGroup = list(cutoffs = cutoffs)
#' rSummary = "mean"
#' bootstrap = N_bootstrap_reps
#' baseArgs = baseArgsFn(e, t, r, tStar, design, riskGroup, rSummary, bootstrap)
#' concordance_fn(baseArgs)
#' @import survival
#' @export

concordance_fn = function(baseArgs){
  eee = baseArgs$e
  ttt = baseArgs$t
  risk = baseArgs$r
  weight = baseArgs$weight
  t_star = baseArgs$tStar
  censoring_event = ifelse(eee == 0, 1, 0)
  survival_object = Surv(time = ttt, event = censoring_event)
  survival_fit = survfit(survival_object ~ 1, weights = weight)
  n_1_uni = which(eee == 1 & ttt <= t_star)
  n_2_uni = 1:length(eee)
  aaa = 1/survival_fn(ttt[n_1_uni], survival_fit) * weight[n_1_uni]

  b1 = if(any(ttt > t_star)){
    as.numeric(ttt > t_star) / survival_fn(t_star, survival_fit)
  } else {
    0
  }
  b2_numerator = as.numeric(ttt <= t_star & eee == 2)
  sss_ttt = survival_fn(ttt, survival_fit)
  b2 = ifelse(b2_numerator == 0, 0, b2_numerator/sss_ttt)
  bbb = (b1 + b2) * weight
  inequality_part = outer(risk[n_1_uni],  risk[n_2_uni], `>`)
  number_part = outer(aaa, bbb)
  numerator = sum(inequality_part * number_part)
  
  denominator = sum(aaa) * sum(bbb)
  concordance = numerator / denominator

  ccc = unique(sort(risk))
  sensitivity = unlist(lapply(ccc, function(this_c){
    sum( aaa[risk[n_1_uni] > this_c] )
  })) / sum(aaa)

  specificity = unlist(lapply(ccc, function(this_c){
    sum( bbb[risk <= this_c] )
  })) / sum(bbb)
  one_minus_specificity = 1 - specificity
  roc_df_0 = unique(data.frame(one_minus_specificity, sensitivity))
  df_for_roc_plot = roc_df_0[order(roc_df_0$one_minus_specificity, roc_df_0$sensitivity), ]
  # roc = list(ccc = ccc,
  #            sensitivity = sensitivity,
  #            specificity = specificity)
  list(concordance = concordance,
       df_for_roc_plot = df_for_roc_plot)
}
