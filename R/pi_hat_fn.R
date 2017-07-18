#' pi_hat_fn
#'
#' For estimating the outcome probability for each risk group
#' defined by \code{baseArgs$k}.  Uses \code{baseArgs$weight}
#' which \code{baseArgsFn} provides for all three 
#' kinds of sampling: random sampling, two-stage sampling,
#' and weighted sampling.  This functionreplaces 
#' \code{piHatFn} which called \code{lambdaHatFn} which
#' throws an error if there are no non-zero events. 
#' \code{pi_hat_fn} allows an estimate to be zero. 
#' \code{Sigma} requires just \code{lambdaFn} 
#' (and not \code{piHatFn}) and therefore for calculating
#' theoretical variances, 
#' rmap requires at least one nonzero event in each 
#' risk group.
#'
#' @param baseArgs A list provided by \code{baseArgsFn}.
#' The objects required here are
#' \code{e}, \code{t},
#' \code{weight}, \code{tStar}, \code{K}, \code{k}, and
#' \code{N_nonzero_events}.
#'
#' @return A vector containing one estimated risk per
#' risk group.
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
#' pi_hat_fn(baseArgs)
#'
#' @export


pi_hat_fn = function(baseArgs){
  eee = ifelse(baseArgs$t > baseArgs$tStar, 0, baseArgs$e)
  ttt = pmin(baseArgs$t, baseArgs$tStar)
  weight = baseArgs$weight
  unlist(lapply(seq(1, baseArgs$K, by = 1), function(kkk){
    if(baseArgs$N_nonzero_events[kkk] == 0) {
      0
    } else {
      e = eee[baseArgs$k == kkk]
      t = ttt[baseArgs$k == kkk]
      w = weight[baseArgs$k == kkk]
      tau = unique(sort(t[e != 0]))
      e_uni = 1:2
      N_at_risk = unlist(lapply(tau, function(this_tau){
        sum(w * as.numeric(t >= this_tau))
      }))
      N_event = lapply(e_uni, function(this_e){
        sapply(tau, function(this_tau){
          sum(w * as.numeric(t == this_tau & e == this_e))
        })
      })
      lambda_hat = lapply(N_event, function(this){
        this / N_at_risk
      })
      one_minus_lambda_hat_dot = 1 - apply(do.call(rbind, lambda_hat), 2, sum)
      the_product = cumprod(c(1, one_minus_lambda_hat_dot[-length(tau)]))
      sum(lambda_hat[[1]] * the_product)
    }
  }))
}
