#' Percentile confidence interval
#'
#' The percentile confidence interval say of size 95% confidence
#' level is just the 0.025 and 0.975 quantiles of the
#' bootstrapped statistics.
#'
#' @param baseArgs A list provided by \code{baseArgsFn}.  The object
#' required by \code{percentile_ci_fn} is simply
#' \code{confidence_level}.
#'
#' @param concordance_boo A vector of real numbers typically obtained
#' from bootstrapping a single statistic.
#'
#' @return A named vector whose elements are named \code{lower} and
#' \code{upper} containing the lower and upper limits of a confidence
#' interval with level \code{baseArgs$confidence_level}
#'
#' @examples
#' set.seed(2)
#' N = 1000
#' x = rnorm(N)
#' N_bootstraps = 1000
#' x_boo = sapply(1:N_bootstraps, function(n_bootstrap){
#'   x_boo = x[sample(1:N, size = N, replace = TRUE)]
#'   mean(x_boo)
#' })
#' baseArgs = list(confidence_level = 0.95)
#' percentile_ci_fn(baseArgs, x_boo)
#' t.test(x)$conf.int
#'
#' @export

percentile_ci_fn = function(baseArgs, concordance_boo){
  alpha = 1 - baseArgs$confidence_level
  ci = quantile(concordance_boo, c(alpha/2, 1 - alpha/2))
  names(ci) = c("lower", "upper")
  ci
}
