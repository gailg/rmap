#' interpolate_one_bootstrap_fn
#' 
#' Interpolate those \code{rho} missing from a bootstrap
#' 
#' @param one_bootstrap A numerical vector of the same
#' length as \code{rho_uni}.  This vector corresponds
#' to one bootstrap replication of \code{pi_hat_nn_fn}.
#' If \code{rho} in \code{rho_uni} appears in the bootstrap
#' \code{one_bootstrap} is equal to \code{pi_hat} at this
#' \code{rho}; if \code{rho} is missing, \code{one_bootstrap}
#' is equal to \code{NA} at this \code{rho}.
#' 
#' @param rho_uni A numerical vector containt the ordered 
#' unique values of assigned risk of the original data.
#' 
#' @return A numerical vector of the same lenght as 
#' \code{rho_uni}. The same vector as \code{one_bootstrap}
#' but with all the \code{NA}s replaced with
#' interpolated values.
#' 
#' @examples 
#' options(digits = 3)
#' set.seed(1)
#' xxx = df_randomSample(10)
#' e = xxx$e
#' t = xxx$t
#' r = round(xxx$r, 2)
#' tStar = 10
#' design = "randomSample"
#' epsilon = nrow(xxx)^(-1/3)
#' riskGroup = list(epsilon = epsilon)
#' rSummary = "mean"
#' bootstrap = 3
#' confidenceLevel = 0.95
#' multicore = FALSE
#' verbose = FALSE
#' baseArgs = baseArgsFn(e, t, r, tStar, design, riskGroup, rSummary, bootstrap,
#'                       confidenceLevel, multicore, verbose)
#' e = baseArgs$e
#' t = baseArgs$t
#' tStar = baseArgs$tStar
#' baseArgs$e = ifelse(t > tStar, 0, e)
#' baseArgs$t = ifelse(t > tStar, tStar, t)
#' estimate = pi_hat_nn_fn(baseArgs)
#' estimate
#' bootstraps_raw = lapply(1:baseArgs$N_bootstraps, function(n_bootstrap) {
#'   baseArgsBoot = baseArgsBootFn(baseArgs)
#'   pi_hat_nn_fn(baseArgsBoot)
#' })
#' bootstraps_raw
#' bootstraps = gather_fn(bootstraps_raw, rho_uni = estimate[, "rho"])
#' bootstraps
#' bootstraps_interpolated = t(apply(bootstraps, 1, interpolateOneBsFn, estimate[, "rho"]))
#' bootstraps_interpolated
#' 
#' @export

interpolate_one_bootstrap_fn = function(one_bootstrap, rho_uni) {
  ans = one_bootstrap
  # Fill in left endpoint(s) with the first non-NA value
  if(is.na(ans[1])) {
    whichLeftEnd = seq_len(which(duplicated(is.na(ans)) == FALSE)[2] - 1)
    ans[whichLeftEnd] = ans[max(whichLeftEnd) + 1]
  } 
  # Fill in the right endpoint(s) with the last non-NA value
  if(is.na(ans[length(ans)])) {
    whichRightEnd = length(ans) - (seq_len(which(duplicated(is.na(rev(ans))) == FALSE)[2] - 1) - 1)
    ans[whichRightEnd] = ans[min(whichRightEnd) - 1]
  }
  # Fill in any NA's in the middle by connecting a line from the first
  # non-NA value on the left to the first non-NA value on the right.
  # Interpolate the y = piHatNN value for this missing NA with x = rho.
  whichMid = which(is.na(ans))
  for(ii in whichMid) {
    # 1.) Find left point.
    left = ii - 1
    while( is.na(ans[left]) ) left = left - 1
    # 2.) Find right point
    right = ii + 1
    while( is.na(ans[right]) ) right = right + 1
    ans[ii] = (((rho_uni[ii] - rho_uni[left]) * (ans[right] - ans[left]) ) /
               (rho_uni[right] - rho_uni[left])) + ans[left]  
  }
  ans
}
