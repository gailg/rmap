#' @title The real workhorse for \code{rmap}
#' 
#' @description Validate a personal risk model using a grouped
#' analysis by comparing assigned risks to subsequent outcomes
#' in risk groups.  Provide an attribute diagram.
#' Also evaluate the discrimination of the
#' personal risk model by calculating the concordance and 
#' providing an roc plot.
#' 
#' 
#' @param baseArgs A list provided by \code{baseArgsFn}.
#' The objects required to run \code{rmap_fn} are
#' \code{c},
#' \code{e},
#' \code{confidence_level},
#' \code{e},
#' \code{K},
#' \code{k},
#' \code{N_bootstraps},
#' \code{N_nonzero_events},
#' \code{N_two_stage},
#' \code{n_two_stage},
#' \code{r},
#' \code{rSummary},
#' \code{sampling},
#' \code{small_number},
#' \code{t},
#' \code{target_category},
#' \code{tStar}, and
#' \code{weight}.
#' 
#' @return A list containing
#' \itemize{
#' \item{\code{plots}: }{A list containing 
#' \itemize{
#' \item{\code{df_for_roc_plot}: }{A data.frame containing the columns
#' \code{one_minus_specificity} and \code{sensitivity}, which can be plotted
#' to produce an roc plot.
#' }
#' \item{\code{roc_plot}: }{An roc plot.
#' }
#' \item{\code{risk_plot}: }{An attribute diagram showing 
#' for each risk group the estimate 
#' \code{pi_hat} of the outcome probability at each summary of 
#' assigned risk.
#' }
#' }
#' }
#' \item{\code{summary}: }{A list containing
#' \itemize{
#' \item{\code{pi_estimate}: }{A data.frame containing 
#' \code{K} rows, one for each risk group and the following columns:
#' \itemize{
#' \item{\code{gamma_hat}: }{The weighted proportion of subjects
#' in each risk group.
#' }
#' \item{\code{r}: }{A summary determined by \code{rSummary}
#' of the assigned risk in each risk group.
#' }
#' \item{\code{pi_hat}: }{The estimated outcome probability
#' in each risk group.
#' }
#' }
#' }
#' \item{\code{pi_sd_theory}: }{A data.frame containing \code{K}
#' rows and the following columns:
#' \itemize{
#' \item{\code{sd}: }{The estimated standard deviation of the 
#' estimate \code{pi_hat} in each risk group
#' obtained by asymptotic theory.
#' }
#' \item{\code{lower}: }{The lower bound of a confidence interval
#' for the outcome probability in each risk group.
#' }
#' \item{\code{upper}: }{The upper bound of the confidence interval.
#' }
#' \item{\code{in_ci}: }{A character "yes" or "no" indicating
#' if \code{pi_estimate$r} falls
#' in the confidence interval.
#' }
#' }
#' }
#' \item{\code{pi_sd_boot}: }{A data.frame containing
#' \itemize{
#' \item{\code{sd_boot}: }{The bootstrap estimate of teh
#' standard deviation of the \code{pi_hat}.
#' }
#' \item{\code{lower}: }{The lower bound of the bootstrap percentile
#' confidence interval.
#' }
#' \item{\code{upper}: }{The upper bound of the bootstrap percentile
#' confidence interval.
#' }
#' \item{\code{in_ci_boot}: }{A character "yes" or "no" indicating if 
#' \code{pi_estimate$r} falls in the bootstrap confidence interval.
#' }
#' }
#' }
#' \item{\code{gof_theory}: }{A data.frame containing
#' \code{statistic}, the chi-square goodness of fit test statistic 
#' using  \code{sd} the asymptotic-theory version of the estimate of
#' standard deviation, and \code{p_value} its p-value.
#' }
#' \item{\code{gof_boot}: }{A data.frame containing
#' \code{statistic}, the chi-square goodness of fit test statistic
#' using \code{sd_boot} the bootstrap version of the estimate of 
#' standard deviation, and \code{p_value} its p-value.
#' }
#' \item{\code{concordance_summary}: }{A data.frame containing the 
#' the concordance estimate 
#' \code{concordance}, together with \code{lower} and \code{upper},
#' the lower and upper bounds of a confidence interval for concordance.
#' }
#' }
#' }
#' }
#' @examples 
#' #-------------------------------------------------- A random sample example
#' set.seed(1)
#' tStar = 10
#' NNN = 100
#' randomSample = df_randomSample(NNN)
#' xxx = randomSample
#' e = xxx$e
#' t = xxx$t
#' r = xxx$r
#' design = "randomSample"
#' riskGroup = list(K = 2)
#' rSummary = "mean"
#' bootstrap = 20
#' set.seed(1)
#' baseArgs = baseArgsFn(e, t, r, tStar, design, riskGroup, rSummary, bootstrap)
#' rmap_1 = rmap_fn(baseArgs)
#' plots = rmap_1$plots
#' grid.arrange(plots$roc_plot, plots$risk_plot, ncol = 2, 
#'              top = "rmap_grouped on a random sample")
#' rmap_1$summary
#' #-------------------------------------------------- A weighted sample example
#' set.seed(2)
#' tStar = 10
#' NNN = 400
#' N_bootstrap_reps = 100
#' cutoffs = c(0, 0.20, 1)
#' weighted_example = weighted_example_fn(NNN)
#' cohort_sampling_probability_dictionary = 
#'    weighted_example$cohort_sampling_probability_dictionary
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
#' rmap_2 = rmap_fn(baseArgs)
#' plots = rmap_2$plots
#' grid.arrange(plots$roc_plot, plots$risk_plot, ncol = 2, 
#'              top = "rmap_grouped on a weighted sample")
#' rmap_2$summary
#' #-------------------------------------------------- A two-stage sample example
#' set.seed(3)
#' tStar = 10
#' NNN = 300
#' twoStageSample = df_twoStage(NNN)
#' xxx = twoStageSample$d
#' e = xxx$e
#' t = xxx$t
#' r = xxx$r
#' N = twoStageSample$N
#' design = list(N_two_stage = N, c = xxx$c)
#' riskGroup = list(K = 3)
#' rSummary = "mean"
#' bootstrap = 100
#' baseArgs = baseArgsFn(e, t, r, tStar, design, riskGroup, rSummary, bootstrap)
#' rmap_3 = rmap_fn(baseArgs)
#' plots = rmap_3$plots
#' grid.arrange(plots$roc_plot, plots$risk_plot, ncol = 2, 
#'              top = "rmap_grouped on a two-stage sample")
#' rmap_3$summary
#' @export

rmap_fn = function(baseArgs){
  concordance_estimate = concordance_estimate_fn(baseArgs)
  concordance_summary = c(estimate = concordance_estimate$concordance)
  df_for_roc_plot = concordance_estimate$df_for_roc_plot
  roc_plot = ggplot(df_for_roc_plot, aes(x = one_minus_specificity, y = sensitivity)) + 
    geom_step() + 
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = 2) 
  
  pi_estimate = pi_estimate_fn(baseArgs)$pi_estimate
  gamma_hat = pi_estimate$gamma_hat
  r_bar = pi_estimate$r
  pi_hat = pi_estimate$pi_hat
  extraArgs = list(gammaHat = gamma_hat,
                   piHat = pi_hat)
  pi_sd_theory = if(any(baseArgs$N_nonzero_events == 0 || baseArgs$sampling == "weighted")){
    NULL
  } else {
    pi_sd_theory_fn(baseArgs, extraArgs)$pi_sd_theory
  }
  gof_theory = if(is.null(pi_sd_theory) || any(pi_sd_theory$sd < baseArgs$small_number)){
    NULL
  } else {
    sigma = pi_sd_theory$sd
    gof_fn(gamma_hat, r_bar, pi_hat, sigma)
  }
  if(baseArgs$N_bootstraps == 0){
    pi_sd_boot = NULL
    gof_boot = NULL
  } else {
    ppp  = pi_sd_boot_fn(baseArgs)
    pi_sd_boot = ppp$pi_sd_boot
    concordance_ci = ppp$concordance_ci
    sigma = pi_sd_boot$sd
    gof_boot = if(any(pi_sd_boot$sd_boot < baseArgs$small_number)){
      NULL
    } else {
      gof_fn(gamma_hat, r_bar, pi_hat, sigma)
    }
    concordance_summary = c(concordance_summary, concordance_ci)
  }
  sd_part = if(baseArgs$sampling == "weighted"){
    if(is.null(pi_sd_boot)){
      NULL
    } else {
      pi_sd_boot[, c("lower", "upper")]
    }
  } else {
    pi_sd_theory[, c("lower", "upper")]
  }
  df_for_risk_plot = if(is.null(sd_part)){
    100 * pi_estimate[, c("r", "pi_hat")]
  } else {
    100 * cbind(pi_estimate[, c("r", "pi_hat")], sd_part)
  }
  risk_plot = 
    ggplot(df_for_risk_plot, aes(x = r, y = pi_hat, ymin = lower, ymax = upper)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = 2) +
    xlim(0,100) + ylim(0, 100) +
    xlab("Assigned Risk (%)") + ylab("Observed Risk (%)")
  if("lower" %in% names(df_for_risk_plot) && "upper" %in% names(df_for_risk_plot)){
    risk_plot = risk_plot + 
      geom_errorbar(aes(ymin = df_for_risk_plot$lower, ymax = df_for_risk_plot$upper), width = 2) 
  }
  plots = list(
    df_for_roc_plot = concordance_estimate$df_for_roc_plot,
    roc_plot = roc_plot,
    risk_plot = risk_plot)
  numerical_summaries = list(
    concordance = concordance_summary,
    gof_asymptotic = gof_theory,
    gof_bootstrap = gof_boot,
    grouped_estimates = pi_estimate,
    grouped_asymptotic_sds = pi_sd_theory,
    grouped_bootstrap_sds = pi_sd_boot

  )
  list(plots = plots,
       numerical_summaries = numerical_summaries)
}