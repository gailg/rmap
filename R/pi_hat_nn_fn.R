pi_hat_nn_fn = function(baseArgs){
  aaa = baseArgs$weight
  GFn = ecdf2Stg(baseArgs$r, aaa)
  rho = sort(unique(baseArgs$r))
  NNs = t(sapply(rho, function(rho1) {
    abs(GFn(baseArgs$r) - GFn(rho1)) < baseArgs$epsilon
  }))
  NNs
  ctr = 0
  if(FALSE){
    kkk = 1
  }
  nearest_neighbor_estimates_0 = lapply(seq(from = 1, to = nrow(NNs), by = 1), function(kkk){
    NN1 = NNs[kkk, ]
    ctr <<- ctr + 1
    if(baseArgs$verbose && (ctr %% 100 == 0))
      print(paste("PID: ", Sys.getpid(), " ", date(), " pi_hat_nn_fn: Iteration ",
                  ctr, " of ", nrow(NNs), sep = ""))
    e = baseArgs$e[NN1]
    t = baseArgs$t[NN1]
    r = baseArgs$r[NN1]
    k = rep(1, length(e))
    weight = baseArgs$weight[NN1]
    tStar = baseArgs$tStar
    riskGroup = list(k = k)
    rSummary = "mean"
    bootstrap = FALSE
    design = if(baseArgs$sampling == "weighted"){
      list(w = weight)
    } else {
      N_two_stage = tapply(aaa[NN1], names(aaa[NN1]), sum)
      c = baseArgs$c[NN1]
      design = list(N_two_stage = N_two_stage, c = c)
    }
    baseArgsNN1 = try(baseArgsFn(e, t, r, tStar, design, riskGroup, rSummary, bootstrap))
    "try-error" %in% class(baseArgsNN1) 
    if( "try-error" %in% class(baseArgsNN1) ){
      print(class(baseArgsNN1))
      "try-error"
    } else {
      pi_hat_fn(baseArgsNN1)
    }
  })
  error = any(unlist(lapply(nearest_neighbor_estimates_0, function(this){
    is.character(this) && this == "try-error"
  })))
  if(error){
    "error"
  } else {
    data.frame(rho, pi_hat = unlist(nearest_neighbor_estimates_0))
  }
}