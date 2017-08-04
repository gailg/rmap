#' @title The workhorse for \code{rmap_individual}
#' 
#' @description Provide the plot and the data for an attribute
#' diagram using an
#' epsilon kernel nearest neighbor estimate of outcome
#' probabilty for each distinct value of assigned risk
#' 
#' @details \code{rmap_individual_fn} produces an individualized
#' attribute diagram for random samples, two-stage samples,
#' and weighted samples.
#' \itemize{
#' \item{Step 1: }{Redefined \code{e} and \code{t} so
#' \code{t} does not exceed \code{tStar}.  If the original 
#' \code{t} exceeds \code{tStar}, redefine it to be 
#' \code{tStar} and reset its corresponding \code{e} to
#' be \code{0} to signify censored.
#' }
#' \item{Step 2: }{calls \code{pi_hat_nn_fn} to get an 
#' estimate which is a data.frame containing the columns:
#' 1) \code{rho} a vector of sorted distinct values of
#' \code{t}, and 2) \code{pi_hat} the kernel nearest
#' neighbor estimate of outcome probability for each 
#' value in \code{rho}.
#' }
#' \item{Step 3: }{calculates a confidence band for 
#' outcome probability at each value of \code{rho}.
#' This confidence band is computed by  bootstrapping the data to 
#' obtain \code{N_bootstrap} replications of nearest neighbor
#' estimates.  Each bootstrap important details:
#' \itemize{
#' \item{1. }{Call \code{base_args_boot_fn} to obtain a resampled
#' data set \code{data.frame(e, t, r, k, weight)}. 
#' }
#' \item{2. }{The bootstrap performs
#' either a two-stage sample or a resampling of targtet_category 
#' as well as resampling the cohort sample.
#' }
#' \item{3. }{\code{weight} is calculated from either the 
#' bootstrapped two-stage
#' sample \code{aaa} or the \code{target_category} and \code{c}
#' column of the bootstrapped cohort sample.
#' }
#' \item{4. }{\code{pi_hat_fn} requires
#' only \code{weight} and not \code{baseArgs$N_two_stage} or
#' \code{baseArgs$target_category}.
#' }
#' }
#' }
#' }
#' 
#' @param baseArgs A list provided by \code{baseArgsFn}.
#' The objects requried to run \code{rmap_individual_fn} are
#' \code{c},
#' \code{e},
#' \code{confidence_level},
#' \code{e},
#' \code{epsilon},
#' \code{K},
#' \code{k},
#' \code{multicore},
#' \code{N_bootstraps}, 
#' \code{N_two_stage},
#' \code{n_two_stage},
#' \code{r},
#' \code{sampling},
#' \code{t},
#' \code{target_category},
#' \code{tStar},
#' \code{verbose}, and
#' \code{weight}.
#' 
#' @return A list containing
#' \itemize{
#' \item{\code{df_for_risk_plot}: }{A data.frame containing the 
#' columns \code{assigned_risk}, \code{observed_risk},
#' and if bootstrap replications have been requested
#' (\code{bootstraps} equals a positive number), \code{lower}
#' and \code{upper}.
#' }
#' \item{\code{risk_plot}: }{An attribute diagram graphic.
#' }
#' }
#' @examples 
#' #-------------------------------------------------- A weighted example
#' data(weighted_example_cohort_sample)
#' xxx = weighted_example_cohort_sample
#' head(xxx)
#' e = xxx$e
#' t = xxx$t
#' r = xxx$r
#' tStar = 10
#' data(weighted_example_target_sample)
#' head(weighted_example_target_sample)
#' target_category = weighted_example_target_sample$category
#' category = xxx$category
#' design = list(target_category = target_category, c = category)
#' epsilon = length(e)^(-1/3)
#' riskGroup = list(epsilon = epsilon)
#' bootstrap = 100
#' set.seed(3)
#' baseArgs = baseArgsFn(e, t, r, tStar, design, riskGroup, rSummary, bootstrap) 
#' rmap_1 = rmap_individual_fn(baseArgs)
#' rmap_1$df_for_risk_plot
#' grid.arrange(rmap_1$risk_plot, top = "rmap_individual_fn on a weighted sample")
#' 
#' @import parallel ggplot2
#' @export
rmap_individual_fn = function(baseArgs){
  e = baseArgs$e
  t = baseArgs$t
  tStar = baseArgs$tStar
  baseArgs$e = ifelse(t > tStar, 0, e)
  baseArgs$t = ifelse(t > tStar, tStar, t)
  estimate = pi_hat_nn_fn(baseArgs)
  confidence_band = if(baseArgs$N_bootstraps == 0){
    NULL
  } else {
    iter_fn = if(baseArgs$multicore) {
      mclapply
    } else {
      lapply
    }
    randomSeeds = sample(1:1e8, baseArgs$N_bootstraps, replace = FALSE)
    bootstraps_raw = lapply(seq(from = 1, to = baseArgs$N_bootstraps, by = 1), function(n_bootstrap){
      if(baseArgs$verbose){
        print(paste("PID: ", Sys.getpid(), " ", date(),
                    " rmap_individual_fn: starting bootstrap ", n_bootstrap, sep = "")) 
      }
      set.seed(randomSeeds[n_bootstrap])
      still_looking = TRUE
      while(still_looking){
        base_args_boot = base_args_boot_fn(baseArgs)
        base_args_boot$boot_code
        still_looking = !(base_args_boot$boot_code == 0)
      }
      pi_hat_nn_fn(base_args_boot)
    })
    bootstraps = gather_fn(bootstraps_raw, estimate[, "rho"])
    bootstraps_interpolated = t(apply(bootstraps, 1, interpolate_one_bootstrap_fn, estimate[, "rho"]))
    bootstraps_interpolated
    confidence_level = baseArgs$confidence_level
    prob_lower = (1 - confidence_level)/2
    prob_upper = 1 - prob_lower
    confidence_band_0 = data.frame(t(apply(
      bootstraps_interpolated, 2, quantile, probs = c(prob_lower, prob_upper))))
    row.names(confidence_band_0) = NULL
    names(confidence_band_0) = c("lower", "upper")
    confidence_band_0
  }
  row.names(estimate) = NULL
  names(estimate) = c("assigned_risk", "observed_risk")
  if(is.null(confidence_band)){
    df_for_risk_plot = 100 * estimate
    risk_plot = ggplot(df_for_risk_plot, aes(x = assigned_risk, y = observed_risk)) +
      geom_line() +
      geom_abline(slope = 1, intercept = 0, color = "red", linetype = 2) +
      xlim(0, 100) + ylim(0, 100) +
      xlab("Assigned Risks (%)") + ylab("Observed Risks (%)")
  } else {
    df_for_risk_plot = 100 * cbind(estimate, confidence_band)
    risk_plot = ggplot(df_for_risk_plot, aes(x = assigned_risk, y = observed_risk, ymin = lower, ymax = upper)) +
      geom_line() +
      geom_ribbon(alpha = 0.3) +
      geom_abline(slope = 1, intercept = 0, color = "red", linetype = 2) +
      xlim(0, 100) + ylim(0, 100) +
      xlab("Assigned Risks (%)") + ylab("Observed Risks (%)")
  }
  list(df_for_risk_plot = df_for_risk_plot,
       risk_plot = risk_plot)
}
