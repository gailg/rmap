

prob_CI_Fn = function(ps, sds) {
  xs = qlogis(ps)
  sd_of_xs = sds / (ps * (1 - ps))
  ci_of_xs = list( lower = xs - 1.96 * sd_of_xs,
                   upper = xs + 1.96 * sd_of_xs)
  structure(do.call(cbind, lapply(ci_of_xs, plogis)), dimnames = list(NULL, c("lower", "upper")))
}
  
# Wed Mar 16 11:04:20 PDT 2011
