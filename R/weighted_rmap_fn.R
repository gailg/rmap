#' The weighted version of rmap
#'
#' @param baseArgs A list provided by \code{baseArgsFn}.
#' The objects required to run \code{weighted_rmap_fn} are
#' \code{e}, \code{t}, \code{r}, \code{c}, \code{k} \code{weight},
#' \code{K}, \code{N_bootstraps},
#' \code{N_nonzero_events}, \code{rSummary}, \code{tStar}
#'
#' @return A list containing the elements
#' \itemize{
#'   \item{\code{concordance_summary}: }{ A one-row data.frame
#'   containing the concordance estimate and the lower and upper
#'   limits of the confidence interval for concordance.
#'   }
#'   \item{\code{df_for_roc_plot}: }{ A data.frame containing the
#'   columns \code{one_minus_specificity} and \code{sensitivity}
#'   which when plotted give the roc plot.
#'   }
#'   \item{\code{gof}: } {A one-row data.frame containing
#'   the goodness-of-fit-statistic and its p_value for testing
#'   if the risk model fits the observed data.
#'   }
#'   \item{\code{pi_summary}: }{ A data.drame containing
#'   \code{K} rows and the columns
#'   \code{gamma_hat} (an estimate of the probability of
#'   falling into each risk group),
#'   \code{r} (the summary statistic, as determined by\code{rSummary}
#'   of the assigned risk of each group),
#'   \code{pi_hat} (the estimated probability of getting the disease
#'   before the end of the study \code{t_star},
#'   \code{sd} (the estimated standard deviation of \code{pi_hat}),
#'   \code{lower} (the lower bound of the bootstrap percentile
#'   confidence interval),
#'   \code{upper} (the upper bound of the bootstrap percentile
#'   confidence interval),
#'   \code{in_ci} (the character string \code{"yes"} or \code{"no"}
#'   indicateing whether or not \code{r} falls in the confidence
#'   interval.
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
#' design = list(targetCategory = target_category, c = cohort_category)
#' riskGroup = list(cutoffs = cutoffs)
#' rSummary = "mean"
#' bootstrap = N_bootstrap_reps
#' baseArgs = baseArgsFn(e, t, r, tStar, design, riskGroup, rSummary, bootstrap)
#' look = weighted_rmap_fn(baseArgs)
#' df_for_roc_plot = look$df_for_roc_plot
#' ggplot(df_for_roc_plot, aes(x = one_minus_specificity, y = sensitivity)) +
#' geom_step() +
#'   xlab("1 - Specificity") +
#'   ylab("Sensitivity")
#' with(look, list(
#'   concordance_summary = concordance_summary,
#'   gof = gof,
#'   pi_summary = pi_summary))
#' @export


weighted_rmap_fn = function(baseArgs){
  concordance = concordance_fn(baseArgs)
  roc = concordance$roc
  concordance = concordance$concordance
  sensitivity = roc$sensitivity
  specificity = roc$specificity
  one_minus_specificity = 1 - specificity
  roc_df_0 = unique(data.frame(one_minus_specificity, sensitivity))
  df_for_roc_plot = roc_df_0[order(roc_df_0$one_minus_specificity, roc_df_0$sensitivity), ]
  gamma_hat = unlist(lapply(seq(1, baseArgs$K, by = 1), function(kkk){
    sum(baseArgs$weight[baseArgs$k == kkk])
  })) / sum(baseArgs$weight)
  pi_hat = pi_hat_fn(baseArgs)
  r_bar = baseArgs$rSummary
  randomSeeds = sample(1:1e8, baseArgs$N_bootstraps, replace = FALSE)
  boo_0 = lapply(1:baseArgs$N_bootstraps, function(n_bootstrap){
    seed = randomSeeds[n_bootstrap]
    base_args_boot = base_args_boot_fn(baseArgs, seed)
    concordance_boo = concordance_fn(base_args_boot)$concordance
    pi_hat_boo = if(base_args_boot$boot_code == 0) {
      pi_hat_fn(base_args_boot)
    } else {
      base_args_boot$boot_message
    }
    list(code = base_args_boot$boot_code,
         concordance = concordance_boo,
         n_bootstrap = n_bootstrap,
         pi_hat = pi_hat_boo)
  })
  code_boo = unlist(lapply(boo_0, `[[`, "code"))
  concordance_boo = unlist(lapply(boo_0, `[[`, "concordance"))
  pi_hat_boo = do.call(rbind, lapply(boo_0, `[[`, "pi_hat")[code_boo == 0])


  concordance_ci = percentile_ci_fn(baseArgs, concordance_boo)
  concordance_ci["lower"]
  pi_ci = data.frame(do.call(rbind, lapply(seq(1, baseArgs$K, by = 1), function(kkk){
    percentile_ci_fn(baseArgs, pi_hat_boo[, kkk])
  })))
  pi_sd = unlist(lapply(seq(1, baseArgs$K, by = 1), function(kkk){
    sd(pi_hat_boo[, kkk])
  }))
  lower = pi_ci$lower
  upper = pi_ci$upper
  pi_in_ci = ifelse(lower <= baseArgs$rSummary & baseArgs$rSummary <= upper, "yes", "no")

  sigma_boo = apply(pi_hat_boo, 2, sd)
  gof_statistic = sum(gamma_hat * (pi_hat - r_bar)^2 / sigma_boo^2)
  p_value = davies(gof_statistic, lambda = gamma_hat)$Qq
  gof = data.frame(gof_statistic = gof_statistic, p_value = p_value)
  concordance_summary = data.frame(100 * round(as.matrix(
    data.frame(concordance, lower = concordance_ci["lower"], upper = concordance_ci["upper"])
  ), 4))
  row.names(concordance_summary) = NULL
  pi_summary = cbind(
    round(as.matrix(cbind(data.frame(gamma_hat, r = r_bar, pi_hat, sd = pi_sd), pi_ci)), 4),
    data.frame(in_ci = pi_in_ci))

  list(
    concordance_summary = concordance_summary,
    df_for_roc_plot = df_for_roc_plot,
    gof = gof,
    pi_summary = pi_summary)
}
