concordance_fn = function(baseArgs){
  eee = baseArgs$e
  ttt = baseArgs$t
  risk = baseArgs$r
  weight = baseArgs$weight
  t_star = baseArgs$tStar
  censoring_event = ifelse(eee == 0, 1, 0)
  survival_object = Surv(time = ttt, event = censoring_event)
  survival_fit = survfit(survival_object ~ 1, weights = weight)
  n_1_uni = which(eee == 1 & ttt <= t_star)
  n_2_uni = 1:length(eee)
  aaa = 1/survival_fn(ttt[n_1_uni], survival_fit) * weight[n_1_uni]
  
  b1 = as.numeric(ttt > t_star) / survival_fn(t_star, survival_fit)
  b2_numerator = as.numeric(ttt <= t_star & eee == 2)
  sss_ttt = survival_fn(ttt, survival_fit)
  b2 = ifelse(b2_numerator == 0, 0, b2_numerator/sss_ttt)
  bbb = (b1 + b2) * weight
  numerator = sum(sapply(1:(length(n_1_uni)), function(kkk){
    sapply(n_2_uni, function(n_2){
      n_1 = n_1_uni[kkk]
      aaa[kkk] * bbb[n_2] * (risk[n_1] > risk[n_2])
    })
  }))
  denominator = sum(aaa) * sum(bbb)
  concordance = numerator / denominator
  
  ccc = unique(sort(risk))
  sensitivity = unlist(lapply(ccc, function(this_c){
    sum( aaa[risk[n_1_uni] > this_c] )
  })) / sum(aaa)
  
  specificity = unlist(lapply(ccc, function(this_c){
    sum( bbb[risk <= this_c] )
  })) / sum(bbb)
  roc = list(ccc = ccc,
             sensitivity = sensitivity,
             specificity = specificity)
  list(concordance = concordance,
       roc = roc)
}