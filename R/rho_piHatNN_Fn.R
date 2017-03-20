## Fri Aug 12 11:28:37 PDT 2011
## Copied from inside
##    trunk/projects/riskModel/53-nearest-neighbor/03-scribbles/17e-destined-for-rmap-nne-mclapply-printing.R
## Tue Aug 30 15:19:10 PDT 2011
##   Instead of returning a data.frame with columns rho and piHatNN, we are going to return a matrix
##   with the same columns.

rho_piHatNN_Fn = function(baseArgs) {
  aaa = (baseArgs$N / baseArgs$n)[baseArgs$c]
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
      design = list(N = tapply(aaa[NN1], names(aaa[NN1]), sum),
        c = baseArgs$c[NN1]),
      riskGroup = list(K = 1),
      rSummary = "mean",
      bootstrap = FALSE)
    piHatFn(baseArgsNN1)
  })
  cbind(rho = rho, piHatNN = NNEs)
}

