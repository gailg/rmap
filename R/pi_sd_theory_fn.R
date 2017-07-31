#' The theoretical variance part of \code{rmap_fn}
#' 
#' For each risk group, 
#' calculate the variance of the outcome probability estimate
#' \code{pi_hat} and produce a confidence interval for the
#' outcome probability.

#' 
#' @param baseArgs A list provided by \code{baseArgsFn}.  The objects
#' required by \code{pi_estimate_fn} are 
#' \code{c}
#' \code{e},
#' \code{K}, \code{k},
#' \code{N_nonzero_events},
#' \code{N_two_stage}, \code{n_two_stage},
#' \code{rSummary},
#' \code{t},
#' \code{tStar}, and
#' \code{weight}
#' 
#' @param extraArgs A list intended to be the same parameter as that
#' with the same name in \code{SigmaFn}.  Defaults to \code{FALSE}
#' 
#' @return A list containing
#' \itemize{
#' \item{\code{pi_sd_theory}: }{A data.frame containing \code{K} rows,
#' one for each risk group, and the following columns:
#' \itemize{
#' \item{\code{sd}: }{The estimated standard deviation of the estimate
#' \code{pi_hat}.
#' }
#' \item{\code{lower}: }{The lower limit of the confidence interval for
#' the outcome probability.
#' }
#' \item{\code{upper}: }{The upper limit of the confidence interval for
#' the outcome probability.
#' }
#' \item{\code{in_ci}: }{A character "yes" or "no" indicating if the summary
#' provided by \code{baseArgs$rSummary} falls in the confidence interval.
#' }
#' }
#' }
#' \item{\code{extraArgs}: }{A list containing 
#' \code{pi_hat}, \code{Sigma}, and \code{sigma}.
#' }
#' }
#' @examples 
#' set.seed(1)
#' twoStageSample = df_twoStage(60)
#' tStar = 10
#' xxx = twoStageSample$d
#' e = xxx$e
#' t = round(xxx$t, 1)
#' r = xxx$r
#' N = twoStageSample$N
#' design = list(N_two_stage = N, c = xxx$c)
#' riskGroup = list(K = 2)
#' rSummary = "mean"
#' bootstrap = 100
#' baseArgs = baseArgsFn(e, t, r, tStar, design, riskGroup, rSummary, bootstrap)
#' pi_sd_theory_fn(baseArgs)
#' 
#' @export

pi_sd_theory_fn = function(baseArgs, extraArgs = FALSE){
  pi_hat = if("pi_hat" %in% names(extraArgs)){
    extraArgs$pi_hat
  } else {
    pi_hat_fn(baseArgs)
  }
  Sigma = SigmaFn(baseArgs, extraArgs)
  sigma = sqrt(sapply(baseArgs$K:(2 * baseArgs$K - 1), function(kkk) Sigma[kkk, kkk]) / sum(baseArgs$N_two_stage))
  piHatCIs = prob_CI_Fn(pi_hat, sigma)
  lower = piHatCIs[, "lower"]
  upper = piHatCIs[, "upper"]
  in_ci = ifelse(lower <= baseArgs$rSummary & baseArgs$rSummary <= upper, "yes", "no")
  pi_sd_theory = data.frame(
    sd = sigma,
    lower,
    upper,
    in_ci)  
  additional = list(pi_hat = pi_hat,
                    Sigma = Sigma,
                    sigma = sigma)
  extraArgs = if(is.list(extraArgs)){
    c(extraArgs, additional)
  } else {
    additional
  }
  list(pi_sd_theory = pi_sd_theory,
       extraArgs = extraArgs)
}