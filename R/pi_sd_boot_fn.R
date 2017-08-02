#' The bootstrap part of \code{rmap_fn}
#' 
#' For each risk group, 
#' calculate the bootstrap variance of the outcome probability estimate
#' \code{pi_hat} and produce a confidence interval for the
#' outcome probability.
#' 
#' @param baseArgs A list provided by \code{baseArgsFn}.  The objects
#' required by \code{pi_sd_boot_fn} are 
#' \code{c}
#' \code{e},
#' \code{K}, \code{k},
#' \code{N_bootstraps},
#' \code{N_nonzero_events},
#' \code{N_two_stage}, \code{n_two_stage},
#' \code{r},
#' \code{rSummary},
#' \code{t},
#' \code{target_category},
#' \code{tStar}, and
#' \code{weight}
#' 
#' @return A list containing
#' \itemize{
#' \item{\code{pi_sd_boot}: }{A data.frame containing \code{K} rows,
#' one for each risk group, and the following columns:
#' \itemize{
#' \item{\code{sd_boot}: }{The bootstrap estimate of teh
#' standard deviation of the \code{pi_hat}.
#' \code{pi_hat}.
#' }
#' \item{\code{lower}: }{The lower limit of the confidence interval for
#' the outcome probability.
#' }
#' \item{\code{upper}: }{The upper limit of the confidence interval for
#' the outcome probability.
#' }
#' \item{\code{in_ci_boot}: }{A character "yes" or "no" indicating if the summary
#' provided by \code{baseArgs$rSummary} falls in the confidence interval.
#' }
#' }
#' }
#' \item{\code{concordance_ci}: }{A named vector containing \code{lower} 
#' and \code{upper} the bounds of a confidence interval for the concordance. 
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
#' pi_sd_boot_fn(baseArgs)
#' 
#' @export
pi_sd_boot_fn = function(baseArgs){
  random_seeds = sample(1:1e8, baseArgs$N_bootstraps, replace = FALSE)
  boo_0 = lapply(1:baseArgs$N_bootstraps, function(n_bootstrap){
    set.seed(random_seeds[n_bootstrap])
    base_args_boot = base_args_boot_fn(baseArgs)
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
  concordance_boo_0 = unlist(lapply(boo_0, `[[`, "concordance"))
  concordance_boo = concordance_boo_0[!is.nan(concordance_boo_0)]
  pi_hat_boo = do.call(rbind, lapply(boo_0, `[[`, "pi_hat")[code_boo == 0])
  concordance_ci = percentile_ci_fn(baseArgs, concordance_boo)
  pi_ci = data.frame(do.call(rbind, lapply(seq(1, baseArgs$K, by = 1), function(kkk){
    percentile_ci_fn(baseArgs, pi_hat_boo[, kkk])
  })))
  pi_sd = unlist(lapply(seq(1, baseArgs$K, by = 1), function(kkk){
    sd(pi_hat_boo[, kkk])
  }))
  lower = pi_ci$lower
  upper = pi_ci$upper
  pi_in_ci = ifelse(lower <= baseArgs$rSummary & baseArgs$rSummary <= upper, "yes", "no")
  sigma = apply(pi_hat_boo, 2, sd)
  pi_sd_boot = data.frame(sd = sigma, 
                          lower = lower, 
                          upper = upper, 
                          in_ci = pi_in_ci)
  list(pi_sd_boot = pi_sd_boot,
       concordance_ci = concordance_ci)
}