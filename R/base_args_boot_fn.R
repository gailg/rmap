#' Set up baseArgs for bootstrapping for \code{weighted_rmap_fn}
#'
#' Provide the params necessary for running \code{concordance_fn}
#' and \code{pi_hat_fn} both of which required a \code{baseArgs}.
#'
#' @param baseArgs A list provided by \code{baseArgsFn}.
#' The objects required here are
#' \code{e}, \code{t}, \code{r}, \code{c}, \code{k}, \code{K}
#' \code{cohort_category}, and \code{target_category}.
#'
#' @param seed An integer to seed this bootstrap.
#'
#' @return A list containing
#' \code{e}, \code{t}, \code{r}, \code{c}, \code{k}, \code{weight},
#' \code{N_nonzero_events}
#' with the same descriptions as in \code{baseArgsFn} but for this
#' bootstrap,
#' \code{K} and \code{tStar} are equal to their counterparts in
#' \code{baseArgs} Also,
#' \itemize{
#'   \item{ \code{boot_code}:
#'   }{
#'   An integer equal to \code{0} if there are no errors on the bootstrap
#'   and equal to \code{1} otherwise.
#'   }
#'   \item{ \code{boot_message}:
#'   }{
#'   A character string describing the reason for the error if any.
#'   }
#'   \item{ \code{bootstrap_sample}:
#'   }{
#'   An integer vector of length \code{length(e)}
#'   containing the indices of the cohort subjects chosen
#'   in this bootstrap sample.
#'   }
#'   \item{ \code{target_sample}:
#'   }{
#'   An integer vector of length \code{length(target_category)}
#'   containing the indices of the target sample chosen
#'   in this bootstrap sample.
#'   }
#'   \item{ \code{category_weights_boo}:
#'   }{
#'   \code{baseArgs$category_weights}, but for the bootstrap sample.
#'   }
#' }
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
#' seed = 1
#' base_args_boot_fn(baseArgs, seed)
#'
#' @export


base_args_boot_fn = function(baseArgs, seed) {
  set.seed(seed)
  N_c = length(baseArgs$c)
  N_p = length(baseArgs$target_category)

  bootstrap_sample = sample(1:N_c, N_c, replace = TRUE)
  target_bootstrap_sample = sample(1:N_p, N_p, replace = TRUE)
  KKK = baseArgs$K
  e_boo = baseArgs$e[bootstrap_sample]
  t_boo = baseArgs$t[bootstrap_sample]
  r_boo = baseArgs$r[bootstrap_sample]
  c_boo = baseArgs$c[bootstrap_sample]
  k_boo = baseArgs$k[bootstrap_sample]
  target_category_boo = baseArgs$target_category[target_bootstrap_sample]
  N_in_risk_group = unlist(lapply(seq(1, KKK, by = 1), function(kkk){
    sum(k_boo == kkk)
  }))
  N_in_risk_group_message = paste(
    "risk groups",
    paste(which(N_in_risk_group == 0), collapse = ", "),
    "are empty")
  N_nonzero_events = unlist(lapply(seq(1, KKK, by = 1), function(kkk){
    e_inside_pi_hat = ifelse(t_boo[k_boo == kkk] > tStar, 0, e_boo[k_boo == kkk])
    sum(e_inside_pi_hat > 0)
  }))

  weight_0 = weight_fn(c_boo, target_category_boo)
  weight_code = weight_0$code
  weight_message = weight_0$message
  category_weights_boo = weight_0$category_weights
  weight_boo = unname(category_weights_boo[c_boo])

  boot_code = if( weight_code == 0 && all(N_in_risk_group > 0) ) {
    0
  } else {
    1
  }
  boot_message = if( weight_code != 0 ){
    weight_message
  } else {
    paste("weight ok but", N_in_risk_group_message)
  }

  base_args_boot = list(e = e_boo,
                        t = t_boo,
                        r = r_boo,
                        c = c_boo,
                        k = k_boo,
                        weight = weight_boo,
                        boot_code = boot_code,
                        boot_message = boot_message,
                        bootstrap_sample = bootstrap_sample,
                        target_bootstrap_sample = target_bootstrap_sample,
                        category_weights = category_weights_boo,
                        K = baseArgs$K,
                        N_nonzero_events = N_nonzero_events,
                        target_category = target_category_boo,
                        tStar = baseArgs$tStar)
  base_args_boot
}
