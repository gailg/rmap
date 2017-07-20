#' Goodness of fit for grouped assigned risks
#' 
#' Compute the goodness of fit statistic
#' and corresponding p-value using
#' \code{davies}
#' 
#' @param gamma_hat A vector of length \code{K} equal to 
#' the estimated probability of landing in each risk group.
#' 
#' @param r_bar A vector of length \code{K} equal to a
#' statistic summarizing the assigned risks in each
#' risk group.
#' 
#' @param pi_hat A vector of length \code{K} equal to
#' the estimated outcome probability in each risk group.
#' 
#' @param sigma A vector of length \code{K} equal to 
#' the estimated standard deviation of the estimate
#' \code{pi_hat}.
#' 
#' @return A named vector containing \code{statistic}, the 
#' goodness of fit statistic
#' and \code{p_value} the associated p-value calculated using
#' \code{davies}
#' 
#' @examples 
#' set.seed(1)
#' twoStageSample = df_twoStage(20)
#' tStar = 10
#' xxx = twoStageSample$d
#' e = xxx$e
#' t = round(xxx$t, 1)
#' r = xxx$r
#' N = twoStageSample$N
#' design = list(N_two_stage = N, c = xxx$c)
#' riskGroup = list(K = 2)
#' rSummary = "mean"
#' bootstrap = FALSE
#' baseArgs = baseArgsFn(e, t, r, tStar, design, riskGroup, rSummary, bootstrap)
#' gamma_hat = gammaHatFn(baseArgs)
#' pi_hat = pi_hat_fn(baseArgs)
#' r_bar = baseArgs$rSummary
#' extraArgs = list(gammaHat = gamma_hat,
#'                  piHat = pi_hat)
#' Sigma = SigmaFn(baseArgs, extraArgs)
#' Sigma # covariance matrix for c(gamma_hat, pi_hat)
#' sigma_squared = sapply(baseArgs$K:(2 * baseArgs$K - 1), function(kkk) {
#'   Sigma[kkk, kkk]
#' }) / sum(baseArgs$N_two_stage)
#' sigma = sigma_squared^(1/2)
#' data.frame(gamma_hat, r_bar, pi_hat, sigma)
#' gof_fn(gamma_hat, r_bar, pi_hat, sigma)
#' # just to double check that this makes sense
#' pi_sd_theory_fn(baseArgs, extraArgs)$pi_sd_theory 
#' 
#' @export




gof_fn = function(gamma_hat, r_bar, pi_hat, sigma) {
  gof_statistic = sum( gamma_hat * (pi_hat - r_bar)^2 / sigma^2 )
  p_value =  davies(gof_statistic, lambda = gamma_hat)$Qq
  c(statistic = gof_statistic, p_value = p_value)
}