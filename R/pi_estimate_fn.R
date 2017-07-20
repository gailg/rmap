#' The risk estimation part of \code{rmap_grouped_fn}
#' 
#' For each risk group, produce \code{gamma_hat}, \code{r}, and \code{pi_hat}.
#' 
#' @param baseArgs A list provided by \code{baseArgsFn}.  The objects
#' required by \code{pi_estimate_fn} are 
#' \code{c}
#' \code{e},
#' \code{K}, \code{k},
#' \code{N_nonzero_events},
#' \code{t},
#' \code{tStar}, and
#' \code{weight}
#' 
#' @return A list (to allow for more exports in the future)
#' that contains one object \code{pi_estimate} which is a data.frame
#' with \code{K} rows (one for each risk group) and the columns 
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
#' pi_estimate_fn(baseArgs)
#' 
#' @export


pi_estimate_fn = function(baseArgs){
  gammaHat = gammaHatFn(baseArgs)
  pi_hat = pi_hat_fn(baseArgs)
  r_bar = baseArgs$rSummary
  list(
    pi_estimate = data.frame(
      gamma_hat = gammaHat,
      r = baseArgs$rSummary,
      pi_hat = pi_hat))
}