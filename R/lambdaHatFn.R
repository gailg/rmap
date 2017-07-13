lambdaHatFn = function(baseArgs) {
  tStar = baseArgs$tStar
  eventUni = 0:2
  structure(lapply(1:baseArgs$K, function(kkk) {
    ind = baseArgs$k == kkk
    t_k = baseArgs$t[ind]
    e_k = baseArgs$e[ind]
    c_k = baseArgs$c[ind]
    weight_k = baseArgs$weight[ind]
    tau = sort(unique(t_k[e_k != 0 & t_k < tStar]))  # GG 2017-05-26 this to compensate for allowing t bigger than tStar
    NAR = sapply(tau, function(tauThis) t_k >= tauThis)
    DDD = lapply(eventUni[-1], function(eee) {
      sapply(tau, function(tauThis) (t_k == tauThis) & (e_k == eee))
    })
    denom = colSums(do.call(rbind,lapply(1:sum(ind), function(index) {
      NAR[index, ] * weight_k[index]
    })))
    lambdaHat = do.call(rbind, lapply(DDD, function(DDD1) {
      colSums(do.call(rbind,lapply(1:sum(ind), function(index) {
        DDD1[index, ] * weight_k[index]
      })))/denom
    }))
    tauNames = paste("tau", seq_along(tau), sep = "")
    DDD = lapply(DDD, function(DDD1) {colnames(DDD1) = tauNames; DDD1})
    colnames(lambdaHat) = colnames(NAR) = tauNames
    rownames(lambdaHat) = paste("event", seq_along(eventUni[-1]), sep = "")
    list(lambdaHat = lambdaHat, NAR = NAR, DDD = DDD, tau = tau, denom = denom)
  }), .Names = paste("k", 1:baseArgs$K, sep = ""))
}