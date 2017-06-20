pi_hat_fn = function(baseArgs){
  eee = ifelse(baseArgs$t > baseArgs$tStar, 0, baseArgs$e)
  ttt = pmin(baseArgs$t, baseArgs$tStar)
  weight = baseArgs$weight
  unlist(lapply(seq(1, baseArgs$K, by = 1), function(kkk){
    if(baseArgs$N_nonzero_events[kkk] == 0) {
      0
    } else {
      e = eee[baseArgs$k == kkk]
      t = ttt[baseArgs$k == kkk]
      w = weight[baseArgs$k == kkk]
      tau = unique(sort(t[e != 0]))
      e_uni = 1:2
      N_at_risk = unlist(lapply(tau, function(this_tau){
        sum(w * as.numeric(t >= this_tau))
      }))
      N_event = lapply(e_uni, function(this_e){
        sapply(tau, function(this_tau){
          sum(w * as.numeric(t == this_tau & e == this_e))
        })
      })
      lambda_hat = lapply(N_event, function(this){
        this / N_at_risk
      })
      one_minus_lambda_hat_dot = 1 - apply(do.call(rbind, lambda_hat), 2, sum)
      the_product = cumprod(c(1, one_minus_lambda_hat_dot[-length(tau)]))
      sum(lambda_hat[[1]] * the_product)
    }
  }))
} 
