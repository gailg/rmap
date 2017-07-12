#' \code{rmap_ungrouped_fn}
#' 
#' \code{rmap} offers an ungrouped summary.  
#' \code{rmap_ungrouped} is the workhorse that produces it.
#' This currently does not work for weighted design.
#' 
#' @param baseArgs A list provided by \code{baseArgsFn}.
#'   The objects required to run \code{rmap_ungrouped}
#'   are \code{e}, \code{t}, \code{r}
#'   \code{c}, \code{confidence_level}
#'   \code{e}, \code{epsilon},
#'   \code{multicore},
#'   \code{N_two_stage}, \code{n_two_stage}, \code{N_bootstraps}
#'   \code{r}, \code{rSummary},
#'   \code{t}, \code{tStar}, and
#'   \code{verbose}
#'   
#' @return A data.frame containing the four columns
#' \itemize{
#' \item{\code{assigned_risk}: }{
#' A vector containing the ordered distinct assigned risks of the data.
#' }
#' \itemize{
#' \item{\code{observed_risk}: }{
#' A vector containing the kernel nearest neighbor estimate of outcome 
#' probability for each value in the \code{assigned_risk} column.
#' }
#' \itemize{
#' \item{\code{lower}: }{
#' The lower bound of a \code{confidenceLevel} confidence interval for
#' the outcome probability at each assigned risk in the
#' \code{assigned_risk} column.
#' }
#' \itemize{
#' \item{\code{upper}: }{
#' The upper bound of a \code{confidenceLevel} confidence interval for
#' the outcome probability at each assigned risk in the
#' \code{assigned_risk} column.
#' }
#' }
#'
#' @examples 
#' #------------------------------------------------------- randomSample
#' set.seed(1)
#' xxx = df_randomSample(200)
#' e = xxx$e
#' t = xxx$t
#' r = xxx$r
#' tStar = 10
#' design = "randomSample"
#' epsilon = nrow(xxx)^(-1/3)
#' riskGroup = list(epsilon = epsilon)
#' rSummary = "mean"
#' bootstrap = 10
#' confidenceLevel = 0.95
#' multicore = FALSE
#' verbose = TRUE
#' baseArgs = baseArgsFn(e, t, r, tStar, design, riskGroup, rSummary, 
#'                       bootstrap, confidenceLevel, multicore, verbose)
#' df_for_risk_plot_0 = rmap_ungrouped_fn(baseArgs)
#' head(df_for_risk_plot_0)
#' 
#' df_for_risk_plot = 100 * df_for_risk_plot_0
#' risk_plot = ggplot(
#'   df_for_risk_plot, 
#'   aes(x = assigned_risk, y = observed_risk, ymin = lower, ymax = upper)) +
#'   geom_line() +
#'   geom_ribbon(alpha = 0.3) +
#'   geom_abline(slope = 1, intercept = 0, color = "red", linetype = 2) +
#'   xlim(0,100) + ylim(0, 100) +
#'   xlab("Assigned Risks (%)") + ylab("Observed Risks (%)")
#' grid.arrange(risk_plot,
#'              top = "rmap ungrouped on random sample")
#' #------------------------------------------------------- two stage sample
#' set.seed(1)
#' twoStageSample = df_twoStage(300)
#' xxx = twoStageSample$d
#' e = xxx$e
#' t = xxx$t
#' r = xxx$r
#' tStar = 10
#' N = twoStageSample$N
#' design = list(N_two_stage = N, c = xxx$c)
#' epsilon = nrow(xxx)^(-1/3)
#' riskGroup = list(epsilon = epsilon)
#' rSummary = "mean"
#' bootstrap = 10
#' confidenceLevel = 0.95
#' multicore = FALSE
#' verbose = TRUE
#' baseArgs = baseArgsFn(e, t, r, tStar, design, riskGroup, rSummary, 
#'                       bootstrap, confidenceLevel, multicore, verbose)
#' df_for_risk_plot_0 = rmap_ungrouped_fn(baseArgs)
#' head(df_for_risk_plot_0)
#' 
#' df_for_risk_plot = 100 * df_for_risk_plot_0
#' risk_plot = ggplot(
#'   df_for_risk_plot, 
#'   aes(x = assigned_risk, y = observed_risk, ymin = lower, ymax = upper)) +
#'   geom_line() +
#'   geom_ribbon(alpha = 0.3) +
#'   geom_abline(slope = 1, intercept = 0, color = "red", linetype = 2) +
#'   xlim(0,100) + ylim(0, 100) +
#'   xlab("Assigned Risks (%)") + ylab("Observed Risks (%)")
#' grid.arrange(risk_plot,
#'              top = "rmap ungrouped on two-stage-sample")
#' @export
rmap_ungrouped_fn = function(baseArgs){
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
      require(parallel)
      mclapply
    } else {
      lapply
    }
    randomSeeds = sample(1:1e8, baseArgs$N_bootstraps, replace = FALSE)
    bootstraps_raw = iter_fn(seq_len(baseArgs$N_bootstraps), function(n_bootstrap) {
      set.seed(randomSeeds[n_bootstrap])
      if(baseArgs$verbose) 
        print(paste("PID: ", Sys.getpid(), " ", date(),
                    " rmap_ungrouped_fn: starting bootstrap ", n_bootstrap, sep = "")) 
      while(TRUE) {
        baseArgsBoot = baseArgsBootFn(baseArgs)
        pi_hat_nn_try = tryCatch(pi_hat_nn_fn(baseArgsBoot),
                                 error = function(pi_hat_nn_try) "error!")
        if( !(is.character(pi_hat_nn_try) && pi_hat_nn_try == "error!") ) {
          break
        } else {
          if(baseArgs$verbose)
            print(paste("PID: ", Sys.getpid(), " ", date(),
                        " rmap_ungrouped_fn: BAD SAMPLE in bootstrap ",
                        n_bootstrap, ". RESAMPLING.", sep = ""))
        }
      }
      pi_hat_nn_try      
    })
    bootstraps = gather_fn(bootstraps_raw, estimate[, "rho"])
    bootstraps_interpolated = t(apply(bootstraps, 1, interpolate_one_bootstrap_fn, estimate[, "rho"]))
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
    estimate
  } else {
    cbind(estimate, confidence_band)
  }
}