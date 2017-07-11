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
#'   \item{\code{concordance_summary}: }{
#'   A named vector containing the concordance estimate and the upper
#'   and lower limits of a confidence interval with confidence level
#'   \code{confidence_level}.
#'   }
#'   \item{\code{df_for_roc_plot}: }{
#'   A data.frame containing columns \code{one_minus_specificity} and
#'   \code{sensitivity} that can be used to produce an roc plot.
#'   }
#'   \item{\code{gof}: }{
#'   A named vector containing the Hosmer-Lemeslow goodness of fit
#'   statistic and its p_value for testing if the data fit the risk 
#'   model.
#'   }
#'   \item{\code{pi_summary}: }{A data.frame containing 
#'   \code{K} rows and the columns:
#'   \itemize{
#'   \item{\code{gamma_hat}: }{An estimate of the probability of
#'   falling into each risk group.
#'   }
#'   \item{\code{r}: }{A summary of the assigned risk 
#'   values falling into each risk group
#'   which summary determined by\code{rSummary}.
#'   }
#'   \item{\code{pi_hat}: }{The estimated probability of getting the disease
#'   before the end of the study \code{t_star} in each risk group.
#'   }
#'   \item{\code{sd}: } {The estimated bootstrap standard deviation of \code{pi_hat}.
#'   }
#'   \item{\code{lowerBoot}: } {The lower bound of the bootstrap percentile
#'   confidence interval.
#'   }
#'   \item{\code{upperBoot}: } {The upper bound of the bootstrap percentile
#'   confidence interval.
#'   }
#'   \item{\code{in_ci}: } {The character string \code{"yes"} or \code{"no"}
#'   indicating whether or not \code{r} falls in the 
#'   confidence interval.
#'   }
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
  gof = c(statistic = gof_statistic, p_value = p_value)
  concordance_summary = unlist(data.frame(100 * round(as.matrix(
    data.frame(concordance, lower = concordance_ci["lower"], upper = concordance_ci["upper"])
  ), 4)))
  pi_summary = cbind(
    round(as.matrix(cbind(data.frame(gamma_hat, r = r_bar, pi_hat, sd = pi_sd), pi_ci)), 4),
    data.frame(in_ci = pi_in_ci))

  list(
    concordance_summary = unlist(concordance_summary),
    df_for_roc_plot = df_for_roc_plot,
    gof = gof,
    pi_summary = pi_summary)
}
