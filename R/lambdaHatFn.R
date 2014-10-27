  lambdaHatFn = function(baseArgs) {
    aaa = baseArgs$N / baseArgs$n
    eventUni = 0:2
    structure(lapply(1:baseArgs$K, function(kkk) {
      ind = baseArgs$k == kkk
      t_k = baseArgs$t[ind]
      e_k = baseArgs$e[ind]
      c_k = baseArgs$c[ind]
      tau = sort(unique(t_k[e_k != 0]))
      NAR = sapply(tau, function(tauThis) t_k >= tauThis)
  
      DDD = lapply(eventUni[-1], function(eee) {
        sapply(tau, function(tauThis) (t_k == tauThis) & (e_k == eee))
      })
  
      denom = colSums(do.call(rbind,lapply(1:sum(ind), function(index) {
        NAR[index, ] * aaa[c_k][index]
      })))
  
      lambdaHat = do.call(rbind, lapply(DDD, function(DDD1) {
        colSums(do.call(rbind,lapply(1:sum(ind), function(index) {
          DDD1[index, ] * aaa[c_k][index]
        })))/denom
      }))
  
      tauNames = paste("tau", seq_along(tau), sep = "")
      DDD = lapply(DDD, function(DDD1) {colnames(DDD1) = tauNames; DDD1})
      colnames(lambdaHat) = colnames(NAR) = tauNames
      rownames(lambdaHat) = paste("event", seq_along(eventUni[-1]), sep = "")
      
      list(lambdaHat = lambdaHat, NAR = NAR, DDD = DDD, tau = tau, denom = denom)
    }), .Names = paste("k", 1:baseArgs$K, sep = ""))
  }
## Gail
## Thu Apr 28 13:03:20 PDT 2011
## The t-sapply in
##     denom = colSums(t(sapply(1:sum(ind), function(index) {
##       NAR[index, ] * aaa[c_k][index]
##     })))
## and 
    ## lambdaHat = t(sapply(DDD, function(DDD1) {
    ##   colSums(t(sapply(1:sum(ind), function(index) {
    ##     DDD1[index, ] * aaa[c_k][index]
    ##   }))) / denom
    ## }))
## does not do the right thing when there is one tau in a risk group.
## For example, where we expect a matrix with one column, t-sapply gives us a vector.
## Replace with do.call-rbind-lapply.
