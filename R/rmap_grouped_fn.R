#' @title The workhorse for the grouped part of \code{rmap}
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
#' The objects required to run \code{rmap_grouped_fn} are
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
#' \item{\code{pi_sd_theory}: }{A data.frame containing
#' \code{sd},
#' \code{lower},
#' \code{upper},
#' and \code{in_ci}.
#' }
#' \item{\code{pi_sd_boot}: }{A data.frame containing
#' \code{sd_boot},
#' \code{lower},
#' \code{upper},
#' and \code{in_ci_boot}
#' }
#' \item{\code{gof_theory}: }{A data.frame containing
#' \code{statistic},
#' and \code{p_value}
#' }
#' \item{\code{gof_boot}: }{A data.frame containing
#' \code{statistic},
#' and \code{p_value}
#' }
#' \item{\code{concordance_summary}: }{A data.frame containing
#' \code{concordance},
#' \code{lower},
#' and \code{upper}
#' }
#' }
#' }
#' }
#' @examples 
#' #-------------------------------------------------- A random sample example
#' set.seed(1)
#' tStar = 10
#' randomSample = df_randomSample(100)
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
#' rmap_1 = rmap_grouped_fn(baseArgs)
#' plots = rmap_1$plots
#' grid.arrange(plots$roc_plot, plots$risk_plot, ncol = 2, 
#'              top = "rmap_grouped on a random sample")
#' rmap_1$summary
#' 
#' @export



rmap_grouped_fn = function(baseArgs){
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
  gof_theory = if(is.null(pi_sd_theory) || pi_sd_theory$sd < baseArgs$small_number){
    NULL
  } else {
    sigma = pi_sd_theory$sd
    gof_fn(gamma_hat, r_bar, pi_hat, sigma)
  }
  ppp  = pi_sd_boot_fn(baseArgs)
  pi_sd_boot = ppp$pi_sd_boot
  concordance_ci = ppp$concordance_ci
  sigma = pi_sd_boot$sd_boot
  gof_boot = if(any(pi_sd_boot$sd_boot < baseArgs$small_number)){
    NULL
  } else {
    gof_fn(gamma_hat, r_bar, pi_hat, sigma)
  }
  concordance_estimate = concordance_estimate_fn(baseArgs)
  concordance_summary = c(concordance = concordance_estimate$concordance,
                          concordance_ci)
  df_for_roc_plot = concordance_estimate$df_for_roc_plot
  roc_plot = ggplot(df_for_roc_plot, aes(x = one_minus_specificity, y = sensitivity)) + 
    geom_step()
  sd_part = if(baseArgs$sampling == "weighted"){
    pi_sd_boot[, c("lower", "upper")]
  } else {
    pi_sd_theory[, c("lower", "upper")]
  }
  df_for_risk_plot = 100 * cbind(pi_estimate[, c("r", "pi_hat")], sd_part)
  risk_plot = ggplot(df_for_risk_plot, aes(x = r, y = pi_hat, ymin = lower, ymax = upper)) +
    geom_point() +
    geom_errorbar(aes(ymin = df_for_risk_plot$lower, ymax = df_for_risk_plot$upper), width = 2) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = 2) +
    xlim(0,100) + ylim(0, 100) +
    xlab("Assigned Risk (%)") + ylab("Observed Risk (%)")
  plots = list(
    df_for_roc_plot = concordance_estimate$df_for_roc_plot,
    roc_plot = roc_plot,
    risk_plot = risk_plot)
  summary = list(pi_estimate = pi_estimate,
                 pi_sd_theory = pi_sd_theory,
                 pi_sd_boot = pi_sd_boot,
                 gof_theory = gof_theory,
                 gof_boot = gof_boot,
                 concordance_summary = concordance_summary
  )
  list(plots = plots,
       summary = summary)
}