#' Calculate the 95 percent confidence interval of a probability using a logit transformation
#' 
#' @param ps A numeric vector of probabilities
#' 
#' @param sds A numeric vector of standard deviations.  The elements
#' in this vector correspond to the elements in \code{ps} above
#' 
#' @export A matrix with two columns \code{lower} and \code{upper}
#' and a row for each element in \code{ps} and \code{sds}.
#' 
#' @examples 
#' prob_CI_Fn(ps = 0.99, sds = 0.01)
#' prob_CI_Fn(ps = c(0.98, 0.99), sds = c(0.01, 0.01))
#' 
#' set.seed(1)
#' someCIs = prob_CI_Fn( ps = (1:99) / 100, sds = runif(99, 0.01, 0.05) )
#' 
#' @export
#' 
prob_CI_Fn = function(ps, sds) {
  xs = qlogis(ps)
  sd_of_xs = sds / (ps * (1 - ps))
  ci_of_xs = list( lower = xs - 1.96 * sd_of_xs,
                   upper = xs + 1.96 * sd_of_xs)
  structure(do.call(cbind, lapply(ci_of_xs, plogis)), dimnames = list(NULL, c("lower", "upper")))
}
  
# Wed Mar 16 11:04:20 PDT 2011
