pi_hat_nn_fn = function(baseArgs) {
  aaa = (baseArgs$N_two_stage / baseArgs$n_two_stage)[baseArgs$c]
  GFn = ecdf2Stg(baseArgs$r, aaa)
  rho = sort(unique(baseArgs$r))
  NNs = t(sapply(rho, function(rho1) {
    abs(GFn(baseArgs$r) - GFn(rho1)) < baseArgs$epsilon
  }))
  ctr = 0
  NNEs = apply(NNs, 1, function(NN1) {
    ctr <<- ctr + 1
    if(baseArgs$verbose && (ctr %% 100 == 0))
      print(paste("PID: ", Sys.getpid(), " ", date(), " rho_piHatNN_Fn: Iteration ",
                  ctr, " of ", nrow(NNs), sep = ""))
    baseArgsNN1 = baseArgsFn(
      e = baseArgs$e[NN1],
      t = baseArgs$t[NN1],
      r = baseArgs$r[NN1],
      tStar = baseArgs$tStar,
      design = list(N_two_stage = tapply(aaa[NN1], names(aaa[NN1]), sum),
                    c = baseArgs$c[NN1]),
      riskGroup = list(K = 1),
      rSummary = "mean",
      bootstrap = FALSE)
    piHatFn(baseArgsNN1)
  })
  data.frame(rho = rho, pi_hat = NNEs)
}