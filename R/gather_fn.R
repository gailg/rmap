#' \code{gather_fn}
#' 
#' Organize bootstrapped nearest neighbor estimates of
#' pi_hats into a matrix so the 
#' there is a row for each bootstrap and a column for
#' each element in the input \code{all_rho}.  In 
#' \code{rmap} this will be the distinct values of
#' assigned risk of the original data.
#' 
#' @param bootstraps_raw A list of data.frames, one
#' data.frame for each bootstrap containing the result
#' of a bootstrapped call to \code{pi_hat_nn_fn}.
#' @param rho_uni A vector of distinct values of all
#' the possible values that the bootstrapped \code{rho}
#' can take. 
#' @return A matrix containing one row for each bootstrap and
#' one column for each value of \code{rho_uni}.  Each bootstrap
#' is a data.frame of columns \code{rho} and \code{pi_hat}.
#' For each row of the data.frame,
#'  match up its\code{rho} with the one inside
#' \code{rho_uni} and plop the corresponding \code{pi_hat} in
#' that column of the matrix. Some values of \code{rho_uni} 
#' will not have a representative in the bootstrap 
#' \code{rho}, and those corresponding columns will get a
#' \code{NA}.  The example below should make this all clear.
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
#' gather_fn(bootstraps_raw, rho_uni = estimate[, "rho"])
#'
#' @export
gather_fn = function(bootstraps_raw, rho_uni) {
  pi_hat_boo = do.call(rbind, lapply(bootstraps_raw, function(one_bootstrap) {
    answer = rep(NA, length(rho_uni))
    answer[rho_uni %in% one_bootstrap[, "rho"]] = one_bootstrap[, "pi_hat"]
    answer
  }))
  colnames(pi_hat_boo) = paste("rho_", seq_along(rho_uni), sep = "")
  rownames(pi_hat_boo) = paste("bs", seq_len(length(bootstraps_raw)), sep = "")
  pi_hat_boo
}